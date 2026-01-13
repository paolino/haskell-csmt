{-# LANGUAGE StrictData #-}

module CSMT.Backend.RocksDB
    ( withRocksDB
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
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Database.KV.Database (Database (..))
import Database.KV.RocksDB.Transaction (mkRocksDBDatabase)
import Database.KV.Transaction
    ( Codecs (..)
    , Column (..)
    , DMap
    , DSum (..)
    , mkCols
    )
import Database.RocksDB
    ( BatchOp (..)
    , ColumnFamily
    , Config (..)
    , DB (..)
    , withDBCF
    )

type RocksDB = ReaderT DB IO

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
    :: StandaloneCodecs k v a
    -> RocksDB (Database RocksDB ColumnFamily (Standalone k v a) BatchOp)
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
