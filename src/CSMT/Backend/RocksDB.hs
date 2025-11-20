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
    , Indirect
    , Key
    , Op (..)
    , QueryCSMT
    , getIndirect
    , putIndirect
    , putKey
    )

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
    , DB
    , get
    , withDB
    , write
    )

type RocksDB = ReaderT DB IO

rocksValueToIndirect :: ByteArray a => ByteString -> Indirect a
rocksValueToIndirect = unsafeEvalGet getIndirect

indirectToRocksValue :: ByteArray a => Indirect a -> ByteString
indirectToRocksValue = evalPutM . putIndirect

mkKey :: Key -> ByteString
mkKey = evalPutM . putKey

rocksDBQuery :: ByteArray a => QueryCSMT RocksDB a
rocksDBQuery k = do
    let rdbk = mkKey k
    db <- ask
    r <- lift $ get db rdbk
    pure $ rocksValueToIndirect <$> r

rocksDBChange :: ByteArray a => Change RocksDB k v a
rocksDBChange kvs = do
    let ops = prepare kvs
    db <- ask
    lift $ write db ops
  where
    prepare :: ByteArray a => [Op k v a] -> [BatchOp]
    prepare = concatMap $ \case
        InsertCSMT k ind ->
            let rdbk = mkKey k
                rdbv = indirectToRocksValue ind
            in  [Put rdbk rdbv]
        DeleteCSMT k ->
            let rdbk = mkKey k
            in  [Del rdbk]
        InsertKV _ _ -> []
        DeleteKV _ -> []

rocksDBBackend :: ByteArray a => Backend RocksDB k v a
rocksDBBackend =
    Backend
        { change = rocksDBChange
        , queryCSMT = rocksDBQuery
        , queryKV = \_ -> pure Nothing
        }

newtype RunRocksDB = RunRocksDB (forall a. RocksDB a -> IO a)

withRocksDB :: FilePath -> (RunRocksDB -> IO b) -> IO b
withRocksDB path action = do
    withDB path config $ \db -> do
        action $ RunRocksDB $ flip runReaderT db

unsafeWithRocksDB :: FilePath -> IO (RunRocksDB, IO ())
unsafeWithRocksDB path = do
    wait <- newEmptyMVar
    dbv <- newEmptyMVar
    done <- newEmptyMVar
    link <=< async $ do
        withDB path config $ \db -> do
            putMVar dbv (RunRocksDB $ flip runReaderT db)
            readMVar wait
        putMVar done ()
    rdb <- readMVar dbv
    let close = putMVar wait ()
    pure (rdb, close >> readMVar done)

config :: Config
config =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Nothing
        , prefixLength = Nothing
        , bloomFilter = False
        }
