module Database.KV.RocksDB.Transaction
    ( runRocksDBTransaction
    , mkRocksDBDatabase
    , mkColumns
    )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap
import Data.GADT.Compare (GCompare)
import Database.KV.Database
    ( Database (..)
    , Pos (..)
    , QueryIterator (..)
    )
import Database.KV.Transaction
    ( Codecs (..)
    , Column (..)
    , Transaction
    , run
    )
import Database.RocksDB
    ( BatchOp (..)
    , ColumnFamily
    , DB (..)
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
    , write
    )

mkColumns :: DB -> DMap d Codecs -> DMap d (Column ColumnFamily)
mkColumns db = snd . DMap.mapAccumLWithKey f (columnFamilies db)
  where
    f (c : cfs) _ codec =
        (cfs, Column c codec)
    f [] _ _ =
        error "mkColumns: not enough column families in DB"

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

runRocksDBTransaction
    :: forall t a
     . GCompare t
    => DB
    -- ^ RocksDB database
    -> DMap t Codecs
    -- ^ Codecs for each column of t
    -> Transaction IO ColumnFamily t BatchOp a
    -> IO a
runRocksDBTransaction db codecs = run $ mkRocksDBDatabase db $ mkColumns db codecs
