module CSMT.Backend.RocksDB.TransactionSpec
    ( spec
    )
where

import Control.Lens (Prism', prism')
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Default (Default (..))
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum (DSum (..))
import Data.GADT.Compare (GCompare (..), GEq (..), GOrdering (..))
import Data.Serialize (getWord64be, putWord64be)
import Data.Serialize.Extra (evalGetM, evalPutM)
import Data.Type.Equality ((:~:) (..))
import Data.Word (Word64)
import Database.KV.RocksDB.Transaction (runRocksDBTransaction)
import Database.KV.Transaction (Codecs (..), KV, insert, query)
import Database.RocksDB (Config (createIfMissing), withDBCF)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)

kvCodec :: Codecs (KV ByteString ByteString)
kvCodec =
    Codecs
        { keyCodec = id
        , valueCodec = id
        }

knCodec :: Codecs (KV ByteString Word64)
knCodec =
    Codecs
        { keyCodec = id
        , valueCodec = word64Prism
        }

word64Prism :: Prism' ByteString Word64
word64Prism = prism' putWord64 getWord64
  where
    putWord64 :: Word64 -> ByteString
    putWord64 = evalPutM . putWord64be

    getWord64 :: ByteString -> Maybe Word64
    getWord64 = evalGetM getWord64be

data C a where
    C_KV :: C (KV ByteString ByteString)
    C_KN :: C (KV ByteString Word64)

instance GCompare C where
    gcompare C_KV C_KV = GEQ
    gcompare C_KN C_KN = GEQ
    gcompare C_KV C_KN = GLT
    gcompare C_KN C_KV = GGT

instance GEq C where
    geq C_KV C_KV = Just Refl
    geq C_KN C_KN = Just Refl
    geq _ _ = Nothing

dmapCodecs :: DMap C Codecs
dmapCodecs =
    DMap.fromList
        [ C_KV :=> kvCodec
        , C_KN :=> knCodec
        ]

spec :: Spec
spec = describe "RocksDB Transaction Backend" $ do
    it "can run a simple transaction" $ do
        x <- withSystemTempDirectory "test-db" $ \fp -> do
            withDBCF fp cfg [("kv", cfg), ("kn", cfg)] $ \db -> do
                runRocksDBTransaction db dmapCodecs $ do
                    insert C_KV "key1" "value1"
                    mv <- query C_KV "key1"
                    insert C_KN "key1" $ case mv of
                        Just v -> fromIntegral $ B.length v
                        Nothing -> 0
                    query C_KN "key1"
        x `shouldBe` Just 6

cfg :: Config
cfg = def{createIfMissing = True}
