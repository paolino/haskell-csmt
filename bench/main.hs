import CSMT.Backend.RocksDB
import CSMT.Hashes (Hash, insertKV)
import Control.DeepSeq (NFData (..))
import Control.Exception (SomeException, catch)
import Control.Monad (forM_)
import Data.String (IsString (..))
import Miniterion
    ( bench
    , bgroup
    , defaultMain
    , envWithCleanup
    , whnfIO
    )
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

data WithRocksDb = WithRocksDb
    { _runRocksDB :: RunRocksDB
    , killRocksDB :: IO ()
    }

instance NFData WithRocksDb where
    rnf (WithRocksDb _r _k) = ()

envCSMT :: FilePath -> Int -> IO WithRocksDb
envCSMT path n = do
    let dbPath = path </> "rocksdb"
    (RunRocksDB run, kill) <- unsafeWithRocksDB dbPath
    let r = run $ do
            let csmt = rocksDBCSMT @Hash
            forM_ [1 .. n] $ \i -> do
                let k = fromString $ show i
                    v = "value"
                insertKV csmt k v
    r `catch` \e -> do
        kill
        error
            $ "Failed to setup CSMT in RocksDB: " ++ show (e :: SomeException)
    pure $ WithRocksDb (RunRocksDB run) kill

insertMany :: WithRocksDb -> Int -> IO ()
insertMany (WithRocksDb (RunRocksDB run) kill) m = do
    let r = run $ do
            let csmt = rocksDBCSMT @Hash
            forM_ [1 .. m] $ \i -> do
                let k = fromString $ show (10000000 + i)
                    v = "value"
                insertKV csmt k v
    r `catch` \e -> do
        kill
        error
            $ "Failed to insert many into CSMT in RocksDB: "
                ++ show (e :: SomeException)

main :: IO ()
main = withSystemTempDirectory "csmt-bench" $ \tmpDir ->
    defaultMain
        [ envWithCleanup (envCSMT tmpDir 100) killRocksDB
            $ \e ->
                bgroup
                    "insertion on 100"
                    [ bench "insert 1" . whnfIO . (`insertMany` 1) $ e
                    ]
        , envWithCleanup (envCSMT tmpDir 1000) killRocksDB
            $ \e ->
                bgroup
                    "insertion on 1000"
                    [ bench "insert 1" . whnfIO . (`insertMany` 1) $ e
                    ]
        , envWithCleanup (envCSMT tmpDir 10000) killRocksDB
            $ \e ->
                bgroup
                    "insertion on 10000"
                    [ bench "insert 1" . whnfIO . (`insertMany` 1) $ e
                    ]
        , envWithCleanup (envCSMT tmpDir 100000) killRocksDB
            $ \e ->
                bgroup
                    "insertion on 100000"
                    [ bench "insert 1" . whnfIO . (`insertMany` 1) $ e
                    ]
        ]
