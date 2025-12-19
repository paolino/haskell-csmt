{-# LANGUAGE StrictData #-}

module CSMT.Backend.RocksDB
    ( rocksDBBackend
    , withRocksDB
    , RocksDB
    , RunRocksDB (..)
    , unsafeWithRocksDB
    , rocksValueToIndirect
    , indirectToRocksValue
    )
where

import CSMT.Interface
    ( Backend (..)
    , Change
    , FromKV (..)
    , Indirect
    , Key
    , Op (..)
    , QueryCSMT
    , getIndirect
    , putIndirect
    , putKey
    )

import CSMT.Hashes (byteStringToKey)
import Control.Concurrent (newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.Async (async, link)
import Control.Monad ((<=<))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.ByteArray (ByteArray)
import Data.ByteString (ByteString)
import Data.Serialize.Extra (evalPutM, unsafeEvalGet)
import Database.RocksDB
    ( BatchOp (..)
    , Config (..)
    , DB (columnFamilies)
    , get
    , getCF
    , withDBCF
    , write
    )

type RocksDB = ReaderT DB IO

rocksValueToIndirect :: ByteArray a => ByteString -> Indirect a
rocksValueToIndirect = unsafeEvalGet getIndirect

indirectToRocksValue :: ByteArray a => Indirect a -> ByteString
indirectToRocksValue = evalPutM . putIndirect

mkKey :: Key -> ByteString
mkKey = evalPutM . putKey

rocksDBQueryCSMT :: ByteArray a => QueryCSMT RocksDB a
rocksDBQueryCSMT k = do
    let rdbk = mkKey k
    db <- ask
    r <- lift $ get db rdbk
    pure $ rocksValueToIndirect <$> r

rocksDBQueryKV :: ByteString -> RocksDB (Maybe ByteString)
rocksDBQueryKV k = do
    db <- ask
    let cfkv = case columnFamilies db of
            [cf] -> cf
            _ -> error "rocksDBQueryKV: unexpected number of column families"
    lift $ getCF db cfkv k

rocksDBChange :: ByteArray a => Change RocksDB ByteString ByteString a
rocksDBChange kvs = do
    db <- ask
    let cfkv = case columnFamilies db of
            [cf] -> cf
            _ -> error "rocksDBChange: unexpected number of column families"
    let ops = prepare cfkv kvs
    lift $ write db ops
  where
    prepare cfkv = concatMap $ \case
        InsertCSMT k ind ->
            let rdbk = mkKey k
                rdbv = indirectToRocksValue ind
            in  [Put rdbk rdbv]
        DeleteCSMT k ->
            let rdbk = mkKey k
            in  [Del rdbk]
        InsertKV k v -> [PutCF cfkv k v]
        DeleteKV k -> [DelCF cfkv k]

rocksDBBackend
    :: ByteArray a
    => (ByteString -> a) -> Backend RocksDB ByteString ByteString a
rocksDBBackend mkA =
    Backend
        { change = rocksDBChange
        , queryCSMT = rocksDBQueryCSMT
        , queryKV = rocksDBQueryKV
        , fromKV =
            FromKV
                { fromK = byteStringToKey
                , fromV = mkA
                }
        }

newtype RunRocksDB = RunRocksDB (forall a. RocksDB a -> IO a)

withRocksDB :: FilePath -> (RunRocksDB -> IO b) -> IO b
withRocksDB path action = do
    withDBCF path configCSMT [("kv", configKV)] $ \db -> do
        action $ RunRocksDB $ flip runReaderT db

unsafeWithRocksDB :: FilePath -> IO (RunRocksDB, IO ())
unsafeWithRocksDB path = do
    wait <- newEmptyMVar
    dbv <- newEmptyMVar
    done <- newEmptyMVar
    link <=< async $ do
        withDBCF path configCSMT [("kv", configKV)] $ \db -> do
            putMVar dbv (RunRocksDB $ flip runReaderT db)
            readMVar wait
        putMVar done ()
    rdb <- readMVar dbv
    let close = putMVar wait ()
    pure (rdb, close >> readMVar done)

configCSMT :: Config
configCSMT =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Just 3
        , prefixLength = Nothing
        , bloomFilter = False
        }

configKV :: Config
configKV =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Just 3
        , prefixLength = Nothing
        , bloomFilter = False
        }
