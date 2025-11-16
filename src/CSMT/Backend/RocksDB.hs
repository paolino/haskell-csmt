{-# LANGUAGE StrictData #-}

module CSMT.Backend.RocksDB
    ( rocksDBCSMT
    , withRocksDB
    , RocksDB
    , RunRocksDB (..)
    , unsafeWithRocksDB
    )
where

import CSMT.Interface
    ( CSMT (..)
    , Change
    , Key
    , Op (..)
    , Query
    )

import CSMT.Backend.RocksDB.Key
import Control.Concurrent (newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.Async (async, link)
import Control.Monad ((<=<))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.ByteArray (ByteArray)
import Data.ByteString (ByteString)
import Database.RocksDB
    ( BatchOp (..)
    , Config (..)
    , DB
    , get
    , withDB
    , write
    )

type RocksDB = ReaderT DB IO

mkKey :: Key -> ByteString
mkKey = rocksPathKeyToRocksKey . keyToRocksPathKey

rocksDBQuery :: ByteArray a => Query RocksDB a
rocksDBQuery k = do
    let rdbk = mkKey k
    db <- ask
    r <- lift $ get db rdbk
    pure $ rocksValueToIndirect <$> r

rocksDBChange :: ByteArray a => Change RocksDB a
rocksDBChange kvs = do
    let ops = prepare kvs
    db <- ask
    lift $ write db ops
  where
    prepare :: ByteArray a => [Op a] -> [BatchOp]
    prepare = fmap $ \case
        Insert k ind ->
            let rdbk = mkKey k
                rdbv = indirectToRocksValue ind
            in  Put rdbk rdbv
        Delete k ->
            let rdbk = mkKey k
            in  Del rdbk

rocksDBCSMT :: ByteArray a => CSMT RocksDB a
rocksDBCSMT =
    CSMT
        { change = rocksDBChange
        , query = rocksDBQuery
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
