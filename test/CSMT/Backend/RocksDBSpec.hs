module CSMT.Backend.RocksDBSpec
    ( spec
    )
where

import CSMT
    ( Backend
    , Proof
    , inserting
    , mkInclusionProof
    , verifyInclusionProof
    )
import CSMT.Backend.RocksDB
    ( RocksDB
    , RunRocksDB (..)
    , withRocksDB
    )
import CSMT.Backend.RocksDB qualified as RocksDB
import CSMT.Deletion (deleting)
import CSMT.Hashes
    ( Hash
    , byteStringToKey
    , generateInclusionProof
    , hashHashing
    , insert
    , mkHash
    , queryKV
    )
import CSMT.Hashes qualified as Hashes
import CSMT.Interface
    ( Indirect (..)
    , Op (..)
    , change
    , queryCSMT
    )
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
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

rocksDBBackend :: Backend RocksDB ByteString ByteString Hash
rocksDBBackend = RocksDB.rocksDBBackend mkHash

tempDB :: (RunRocksDB -> IO a) -> IO a
tempDB action = withSystemTempDirectory "rocksdb-test"
    $ \dir -> do
        let path = dir </> "testdb"
        withRocksDB path action

iM :: ByteString -> ByteString -> RocksDB ()
iM = inserting rocksDBBackend hashHashing

dM :: ByteString -> RocksDB ()
dM = deleting rocksDBBackend hashHashing

pfM :: ByteString -> RocksDB (Maybe (Proof Hash))
pfM = mkInclusionProof rocksDBBackend

vpfM :: ByteString -> ByteString -> RocksDB Bool
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
                        let kvs =
                                zip keys
                                    $ BC.pack . show <$> [1 :: Int ..]
                            (testKey, testValue) = kvs !! m
                        run $ do
                            traverse_ (uncurry iM) kvs
                        r <- run (vpfM testKey testValue)
                        r `shouldBe` True

genSomePaths :: Int -> Gen [ByteString]
genSomePaths n = fmap nub <$> listOf1 $ do
    let go 0 = return []
        go c = do
            d <- elements [0 .. 255]
            ds <- go (c - 1)
            return (d : ds)
    B.pack <$> go (n `div` 8)

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
                            , value = mkHash "my-csmt-value"
                            }
                change rocksDBBackend [InsertCSMT [] v]
                r <- rocksDBBackend `queryCSMT` []
                liftIO $ r `shouldBe` Just v
                change rocksDBBackend [DeleteCSMT []]
                r2 <- rocksDBBackend `queryCSMT` []
                liftIO $ r2 `shouldBe` Nothing
        it "can insert and retrieve a key-value pair and delete it"
            $ \(RunRocksDB run) -> run $ do
                let k = "my-key"
                    v = "my-value"
                change rocksDBBackend [InsertKV k v]
                r <- rocksDBBackend `queryKV` k
                liftIO $ r `shouldBe` Just v
                change rocksDBBackend [DeleteKV k]
                r2 <- rocksDBBackend `queryKV` k
                liftIO $ r2 `shouldBe` Nothing
        it "cannot retrieve a non existent key"
            $ \(RunRocksDB run) -> run $ do
                change
                    rocksDBBackend
                    [ InsertCSMT
                        (byteStringToKey "a")
                        (Indirect [] (mkHash "v"))
                    ]
                r <- rocksDBBackend `queryCSMT` byteStringToKey "aa"
                liftIO $ r `shouldBe` Nothing
        it "can insert a key value with csmt" $ \(RunRocksDB run) -> run $ do
            insert rocksDBBackend "my-csmt-key" "my-csmt-value"
            mv <- queryKV rocksDBBackend "my-csmt-key"
            liftIO $ mv `shouldBe` Just "my-csmt-value"
            r <- generateInclusionProof rocksDBBackend "my-csmt-key"
            case r of
                Nothing -> error "expected inclusion proof"
                Just pf -> do
                    verified <-
                        Hashes.verifyInclusionProof
                            rocksDBBackend
                            "my-csmt-value"
                            pf
                    liftIO $ verified `shouldBe` True
        it "verifies a fact" $ \(RunRocksDB run) -> run $ do
            iM "key1" "value1"
            r <- vpfM "key1" "value1"
            liftIO $ r `shouldBe` True
        it "rejects an incorrect fact" $ \(RunRocksDB run) -> run $ do
            iM "key2" "value2"
            r <- vpfM "key2" "wrongvalue"
            liftIO $ r `shouldBe` False
        it "rejects a deleted fact" $ \(RunRocksDB run) -> run $ do
            iM "key3" "value3"
            dM "key3"
            r <- vpfM "key3" "value3"
            liftIO $ r `shouldBe` False
        it "verifies random facts in a sparse tree"
            $ property . testRandomFactsInASparseTree
