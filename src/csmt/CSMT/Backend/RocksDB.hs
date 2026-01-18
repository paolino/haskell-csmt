{-# LANGUAGE StrictData #-}

module CSMT.Backend.RocksDB
    ( withRocksDB
    , mkRocksDBDatabase
    , RocksDB
    , RunRocksDB (..)
    , unsafeWithRocksDB
    , standaloneRocksDBDatabase
    )
where

import CSMT.Backend.Standalone
    ( Standalone (..)
    , StandaloneCodecs (..)
    )
import CSMT.Interface (csmtCodecs)
import Control.Concurrent (newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.Async (async, link)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Database.KV.Database
    ( Database (..)
    , Pos (..)
    , QueryIterator (..)
    )
import Database.KV.Transaction
    ( Codecs (..)
    , Column (..)
    , DMap
    , DSum (..)
    , mkCols
    )
import Database.RocksDB
    ( BatchOp (DelCF, PutCF)
    , ColumnFamily
    , Config (..)
    , DB (DB, columnFamilies)
    , createIterator
    , destroyIterator
    , getCF
    , iterEntry
    , iterFirst
    , iterLast
    , iterNext
    , iterPrev
    , iterSeek
    , iterValid
    , withDBCF
    , write
    )

type RocksDB = ReaderT DB IO

mkRocksDBDatabase
    :: MonadIO m
    => DB
    -> DMap t (Column ColumnFamily)
    -> Database m ColumnFamily t BatchOp
mkRocksDBDatabase db columns =
    Database
        { valueAt = \cf k -> do
            getCF db cf k
        , applyOps = \ops -> do
            write db ops
        , mkOperation = \cf k mv ->
            case mv of
                Just v -> PutCF cf k v
                Nothing -> DelCF cf k
        , columns
        , newIterator = \cf -> do
            i <- createIterator db $ Just cf
            return
                $ QueryIterator
                    { isValid = liftIO $ iterValid i
                    , entry = liftIO $ iterEntry i
                    , step = \pos -> liftIO $ case pos of
                        PosFirst -> iterFirst i
                        PosLast -> iterLast i
                        PosNext -> iterNext i
                        PosPrev -> iterPrev i
                        PosAny k -> iterSeek i k
                        PosDestroy -> destroyIterator i
                    }
        }

standaloneRocksDBCols
    :: StandaloneCodecs k v a
    -> [ColumnFamily]
    -> DMap (Standalone k v a) (Column ColumnFamily)
standaloneRocksDBCols
    StandaloneCodecs{keyCodec, valueCodec, nodeCodec = pa}
    [kvcf, csmtcf] =
        mkCols
            [ StandaloneKVCol
                :=> Column
                    { family = kvcf
                    , codecs = Codecs{keyCodec, valueCodec}
                    }
            , StandaloneCSMTCol
                :=> Column
                    { family = csmtcf
                    , codecs = csmtCodecs pa
                    }
            ]
standaloneRocksDBCols _ _ = error "pureCols: expected exactly two column families"

standaloneRocksDBDatabase
    :: MonadIO m
    => StandaloneCodecs k v a
    -> RocksDB (Database m ColumnFamily (Standalone k v a) BatchOp)
standaloneRocksDBDatabase codecs = do
    db@DB{columnFamilies} <- ask
    pure
        $ mkRocksDBDatabase db (standaloneRocksDBCols codecs columnFamilies)

newtype RunRocksDB = RunRocksDB (forall a. RocksDB a -> IO a)

withRocksDB
    :: FilePath
    -> Int
    -> Int
    -> (RunRocksDB -> IO b)
    -> IO b
withRocksDB path csmtMaxFiles kvMaxFiles action = do
    withDBCF
        path
        def
        [("kv", configKV kvMaxFiles), ("csmt", configCSMT csmtMaxFiles)]
        $ \db -> do
            action $ RunRocksDB $ flip runReaderT db

unsafeWithRocksDB :: FilePath -> Int -> Int -> IO (RunRocksDB, IO ())
unsafeWithRocksDB path csmtMaxFiles kvMaxFiles = do
    wait <- newEmptyMVar
    dbv <- newEmptyMVar
    done <- newEmptyMVar
    link <=< async $ do
        withDBCF
            path
            def
            [("kv", configKV kvMaxFiles), ("csmt", configCSMT csmtMaxFiles)]
            $ \db -> do
                putMVar dbv (RunRocksDB $ flip runReaderT db)
                readMVar wait
        putMVar done ()
    rdb <- readMVar dbv
    let close = putMVar wait ()
    pure (rdb, close >> readMVar done)

def :: Config
def =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Nothing
        , prefixLength = Nothing
        , bloomFilter = False
        }

configCSMT :: Int -> Config
configCSMT n =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Just n
        , prefixLength = Nothing
        , bloomFilter = False
        }

configKV :: Int -> Config
configKV n =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Just n
        , prefixLength = Nothing
        , bloomFilter = False
        }
