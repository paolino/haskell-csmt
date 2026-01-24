module CSMT.Backend.RocksDB.TransactionSpec
    ( spec
    )
where

import Control.Lens (prism')
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Default (Default (..))
import Data.Serialize (getWord64be, putWord64be)
import Data.Serialize.Extra (evalGetM, evalPutM)
import Data.Type.Equality ((:~:) (..))
import Data.Word (Word64)
import Database.KV.Cursor
    ( Entry (..)
    , firstEntry
    , lastEntry
    , nextEntry
    , prevEntry
    )
import Database.KV.Database (mkColumns)
import Database.KV.RocksDB (mkRocksDBDatabase)
import Database.KV.Transaction
    ( Codecs (..)
    , DMap
    , DSum (..)
    , GCompare (..)
    , GEq (..)
    , GOrdering (..)
    , KV
    , Transaction
    , fromPairList
    , insert
    , iterating
    , query
    , runTransactionUnguarded
    )
import Database.RocksDB
    ( BatchOp
    , ColumnFamily
    , Config (createIfMissing)
    , DB (..)
    , withDBCF
    )
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)

-- Codecs for ByteString key-value pairs
kvCodec :: Codecs (KV ByteString ByteString)
kvCodec = Codecs{keyCodec = id, valueCodec = id}

-- Codecs for ByteString key and Word64 value pairs
knCodec :: Codecs (KV ByteString Word64)
knCodec =
    Codecs
        { keyCodec = id
        , valueCodec =
            prism'
                (evalPutM . putWord64be)
                (evalGetM getWord64be)
        }

-- GADT to select between different tables
data Tables a where
    Words :: Tables (KV ByteString ByteString)
    Lenghts :: Tables (KV ByteString Word64)

-- these instances are required for using Tables as a GADT key in a DMap
-- see https://hackage.haskell.org/package/dependent-map
-- they could be derived via TH
instance GCompare Tables where
    gcompare Words Words = GEQ
    gcompare Lenghts Lenghts = GEQ
    gcompare Words Lenghts = GLT
    gcompare Lenghts Words = GGT

instance GEq Tables where
    geq Words Words = Just Refl
    geq Lenghts Lenghts = Just Refl
    geq _ _ = Nothing

-- index codecs by table type
codecs :: DMap Tables Codecs
codecs =
    fromPairList
        [ Words :=> kvCodec
        , Lenghts :=> knCodec
        ]

runRocksDBTransaction
    :: DB
    -> DMap Tables Codecs
    -> Transaction IO ColumnFamily Tables BatchOp a
    -> IO a
runRocksDBTransaction db cols tx = do
    let rocksDBDatabase = mkRocksDBDatabase db $ mkColumns (columnFamilies db) cols
    runTransactionUnguarded rocksDBDatabase tx

spec :: Spec
spec = describe "RocksDB Transaction Backend" $ do
    it "can run a simple transaction" $ do
        result <- withSystemTempDirectory "test-db" $ \fp -> do
            withDBCF fp cfg [("words", cfg), ("lengths", cfg)] $ \db -> do
                runRocksDBTransaction db codecs $ do
                    insert Words "a" "apple"
                    mv <- query Words "a"
                    insert Lenghts "a" $ case mv of
                        Just v -> fromIntegral $ B.length v
                        Nothing -> 0
                    query Lenghts "a"
        result `shouldBe` Just 5
    it
        "can iterate over a table and build while building transaction with them"
        $ do
            result <- withSystemTempDirectory "test-db" $ \fp -> do
                withDBCF fp cfg [("words", cfg), ("lengths", cfg)] $ \db -> do
                    let transact
                            :: Transaction IO ColumnFamily Tables BatchOp a -> IO a
                        transact = runRocksDBTransaction db codecs
                        insertLength Nothing = pure ()
                        insertLength (Just (Entry x v)) =
                            lift
                                $ insert Lenghts x
                                $ fromIntegral
                                $ B.length v
                    -- Insert some key-value pairs bby building a transaction over the KV table
                    transact $ do
                        insert Words "a" "apple"
                        insert Words "b" "banana"
                        insert Words "c" "carrot"
                    -- Iterate over the key-value while building a transaction on the length table
                    transact $ do
                        iterating Words $ do
                            firstEntry >>= insertLength
                            nextEntry >>= insertLength
                            nextEntry >>= insertLength
                            nextEntry >>= insertLength
                    -- Finally, iterate backwards to get the entries, no transaction
                    transact $ do
                        iterating Lenghts $ do
                            x <- lastEntry
                            y <- prevEntry
                            z <- prevEntry
                            l <- prevEntry
                            pure (x, y, z, l)
            result
                `shouldBe` ( Just $ Entry{entryKey = "c", entryValue = 6}
                           , Just $ Entry{entryKey = "b", entryValue = 6}
                           , Just $ Entry{entryKey = "a", entryValue = 5}
                           , Nothing
                           )

cfg :: Config
cfg = def{createIfMissing = True}
