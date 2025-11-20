module CSMT.Backend.RocksDBSpec
    ( spec
    )
where

import CSMT
    ( Key
    , Proof
    , inserting
    , mkInclusionProof
    , verifyInclusionProof
    )
import CSMT.Backend.RocksDB
    ( RocksDB
    , RunRocksDB (..)
    , rocksDBBackend
    , withRocksDB
    )
import CSMT.Deletion (deleting)
import CSMT.Hashes (Hash, hashHashing, mkHash)
import CSMT.Interface
    ( Direction (..)
    , Indirect (..)
    , Op (..)
    , change
    , queryCSMT
    )
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteArray (ByteArray)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Foldable (traverse_)
import Data.List (nub)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, around, describe, it, shouldBe)
import Test.QuickCheck
    ( Gen
    , Property
    , Testable (property)
    , elements
    , forAll
    , listOf
    , listOf1
    )

tempDB :: (RunRocksDB -> IO a) -> IO a
tempDB action = withSystemTempDirectory "rocksdb-test"
    $ \dir -> do
        let path = dir </> "testdb"
        withRocksDB path action

iM :: Key -> Hash -> RocksDB ()
iM = inserting rocksDBBackend hashHashing

dM :: Key -> RocksDB ()
dM = deleting (rocksDBBackend @Hash) hashHashing

pfM
    :: ByteArray a
    => Key
    -> RocksDB (Maybe (Proof a))
pfM = mkInclusionProof rocksDBBackend

vpfM :: Key -> Hash -> RocksDB Bool
vpfM k v = do
    mp <- pfM k
    case mp of
        Nothing -> pure False
        Just p -> verifyInclusionProof rocksDBBackend hashHashing v p

testRandomFactsInASparseTree
    :: RunRocksDB
    -> Property
testRandomFactsInASparseTree (RunRocksDB run) =
    forAll (elements [128 .. 256])
        $ \n -> forAll (genSomePaths n)
            $ \keys -> forAll (listOf $ elements [0 .. length keys - 1])
                $ \ks -> forM_ ks
                    $ \m -> do
                        let kvs = zip keys $ mkHash . BC.pack . show <$> [1 :: Int ..]
                            (testKey, testValue) = kvs !! m
                        run $ do
                            traverse_ (uncurry iM) kvs
                        r <- run (vpfM testKey testValue)
                        r `shouldBe` True

genSomePaths :: Int -> Gen [Key]
genSomePaths n = fmap nub <$> listOf1 $ do
    let go 0 = return []
        go c = do
            d <- elements [L, R]
            ds <- go (c - 1)
            return (d : ds)
    go n

spec :: Spec
spec = around tempDB $ do
    describe "RocksDB CSMT backend" $ do
        it "can initialize and close a db"
            $ \_run -> pure @IO ()
        it "can insert a csmt node and retrieve it and delete it"
            $ \(RunRocksDB run) -> run $ do
                let v =
                        Indirect
                            { jump = []
                            , value = "test value" :: ByteString
                            }
                change rocksDBBackend [InsertCSMT [] v]
                r <- rocksDBBackend `queryCSMT` []
                liftIO $ r `shouldBe` Just v
                change (rocksDBBackend @ByteString) [DeleteCSMT []]
                r2 <- (rocksDBBackend @ByteString) `queryCSMT` []
                liftIO $ r2 `shouldBe` Nothing

        it "verifies a fact" $ \(RunRocksDB run) -> run $ do
            iM [L] $ mkHash "value1"
            r <- vpfM [L] $ mkHash "value1"
            liftIO $ r `shouldBe` True
        it "rejects an incorrect fact" $ \(RunRocksDB run) -> run $ do
            iM [R] $ mkHash "value2"
            r <- vpfM [R] $ mkHash "wrongvalue"
            liftIO $ r `shouldBe` False
        it "rejects a deleted fact" $ \(RunRocksDB run) -> run $ do
            iM [L, R] $ mkHash "value3"
            dM [L, R]
            r <- vpfM [L, R] $ mkHash "value3"
            liftIO $ r `shouldBe` False
        it "verifies random facts in a sparse tree"
            $ property . testRandomFactsInASparseTree
