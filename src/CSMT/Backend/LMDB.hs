{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE StrictData #-}

module CSMT.Backend.LMDB
    ( LMDB
    , lmDBBackend
    , RunLMDB (..)
    , withLMDB
    )
where

import CSMT.Hashes (byteStringToKey)
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
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.ByteArray (ByteArray)
import Data.ByteString (ByteString)
import Data.Serialize.Extra (evalPutM, unsafeEvalGet)
import Database.LMDB.Simple
    ( Environment
    , Limits (..)
    , ReadWrite
    , defaultLimits
    , get
    , getDatabase
    , openReadWriteEnvironment
    , put
    , transaction
    )

type LMDB = ReaderT (Environment ReadWrite) IO

lmValueToIndirect :: ByteArray a => ByteString -> Indirect a
lmValueToIndirect = unsafeEvalGet getIndirect

indirectToLMValue :: ByteArray a => Indirect a -> ByteString
indirectToLMValue = evalPutM . putIndirect

mkKey :: Key -> ByteString
mkKey = evalPutM . putKey

lmDBQueryCSMT :: ByteArray a => QueryCSMT LMDB a
lmDBQueryCSMT k = do
    let rdbk = mkKey k
    env <- ask
    r <- lift $ transaction @ReadWrite env $ do
        db <- getDatabase (Just "csmt")
        get db rdbk
    pure $ lmValueToIndirect <$> r

lmDBQueryKV :: ByteString -> LMDB (Maybe ByteString)
lmDBQueryKV k = do
    env <- ask
    lift $ transaction @ReadWrite env $ do
        db <- getDatabase (Just "kv")
        get db k

lmDBChange :: ByteArray a => Change LMDB ByteString ByteString a
lmDBChange kvs = do
    env <- ask
    let ops = mapM_ prepare kvs
    lift $ transaction env ops
  where
    prepare (InsertKV k v) = do
        db <- getDatabase (Just "kv")
        put db k $ Just v
    prepare (DeleteKV k) = do
        db <- getDatabase (Just "kv")
        put db k (Nothing :: Maybe ByteString)
    prepare (InsertCSMT k ind) = do
        db <- getDatabase (Just "csmt")
        let rdbk = mkKey k
            rdbv = indirectToLMValue ind
        put db rdbk $ Just rdbv
    prepare (DeleteCSMT k) = do
        db <- getDatabase (Just "csmt")
        let rdbk = mkKey k
        put db rdbk (Nothing :: Maybe ByteString)

lmDBBackend
    :: ByteArray a
    => (ByteString -> a)
    -> Backend LMDB ByteString ByteString a
lmDBBackend mkA =
    Backend
        { change = lmDBChange
        , queryCSMT = lmDBQueryCSMT
        , queryKV = lmDBQueryKV
        , fromKV =
            FromKV
                { fromK = byteStringToKey
                , fromV = mkA
                }
        }

newtype RunLMDB = RunLMDB (forall a. LMDB a -> IO a)

withLMDB :: FilePath -> (RunLMDB -> IO b) -> IO b
withLMDB path action = do
    env <-
        openReadWriteEnvironment
            path
            defaultLimits{maxDatabases = 2, mapSize = 10_000_000_000}
    action (RunLMDB $ flip runReaderT env)
