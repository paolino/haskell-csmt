module Database.KV.Transaction
    ( -- * Columns and Codecs
      Codecs (..)
    , Column (..)
    , Selector
    , KeyOf
    , ValueOf
    , KV

      -- * Transaction monadic context
    , Database (..)
    , Context

      -- * Transaction program instructions and monad
    , Instruction
    , Transaction
    , query
    , insert
    , delete

      -- * Transaction interpreter in the context
    , interpretTransaction
    , run

      -- * Reexport
    , module Data.GADT.Compare
    , module Data.Dependent.Map
    , module Data.Dependent.Sum
    , mkCols
    )
where

import Control.Lens (Prism', preview, review)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Operational
    ( ProgramT
    , ProgramViewT (..)
    , singleton
    , viewT
    )
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Control.Monad.Trans.State.Strict
    ( StateT (..)
    , get
    , modify
    )
import Data.ByteString (ByteString)
import Data.Dependent.Map (DMap, fromList)
import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum (DSum ((:=>)))
import Data.GADT.Compare (GCompare (..), GEq (..), GOrdering (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | Column definition for key-value pairs. This is a placeholder for the 2 types k v.
-- It is needed as a single type index.
data KV k v

-- | Selector type for columns
type Selector t k v = t (KV k v)

-- | Project key type from column
type family KeyOf c where
    KeyOf (KV k v) = k

-- | Project value type from column
type family ValueOf c where
    ValueOf (KV k v) = v

-- | Codecs for encoding/decoding keys and values
data Codecs c = Codecs
    { keyCodec :: Prism' ByteString (KeyOf c)
    , valueCodec :: Prism' ByteString (ValueOf c)
    }

-- | Column definition
data Column cf c = Column
    { family :: cf
    , codecs :: Codecs c
    }

-- throw should never happen
decodeValueThrow
    :: MonadFail m => Codecs c -> ByteString -> m (ValueOf c)
decodeValueThrow codec bs =
    case preview (valueCodec codec) bs of
        Just v -> return v
        Nothing -> fail "Failed to decode value"

-- | DB 'interface
data Database m cf t op = Database
    { valueAt :: cf -> ByteString -> m (Maybe ByteString)
    , applyOps :: [op] -> m ()
    , mkOperation :: cf -> ByteString -> Maybe ByteString -> op
    , columns :: DMap t (Column cf)
    }

-- | Workspace for a single column, this iis where the changes are stored
newtype Workspace c = Workspace (Map (KeyOf c) (Maybe (ValueOf c)))

-- modify workspace
overWorkspace
    :: (Map (KeyOf c) (Maybe (ValueOf c)) -> Map (KeyOf c) (Maybe (ValueOf c)))
    -> Workspace c
    -> Workspace c
overWorkspace f (Workspace ws) = Workspace (f ws)

-- | All workspaces for all columns
type Workspaces t = DMap t Workspace

-- | Monad that read the DB before the transaction and modifies the workspaces
newtype Context m cf t op a = Context
    { unContext
        :: StateT (Workspaces t) (ReaderT (Database m cf t op) m) a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadFail
        , MonadIO
        )

-- | Instructions for the transaction
data Instruction t a where
    Query
        :: (GCompare t, Ord (KeyOf c))
        => t c
        -> KeyOf c
        -> Instruction t (Maybe (ValueOf c))
    Insert
        :: (GCompare t, Ord (KeyOf c))
        => t c
        -> KeyOf c
        -> ValueOf c
        -> Instruction t ()
    Delete
        :: (GCompare t, Ord (KeyOf c)) => t c -> KeyOf c -> Instruction t ()

-- | Transaction operational monad
type Transaction m cf t op =
    ProgramT (Instruction t) (Context m cf t op)

-- | Query a value for the given key in the given column
query
    :: (GCompare t, Ord (KeyOf c))
    => t c
    -- ^ column
    -> KeyOf c
    -- ^ key
    -> Transaction m cf t op (Maybe (ValueOf c))
query t k = singleton $ Query t k

-- | Insert a value for the given key in the given column
insert
    :: (GCompare t, Ord (KeyOf c))
    => t c
    -- ^ column
    -> KeyOf c
    -- ^ key
    -> ValueOf c
    -- ^ value
    -> Transaction m cf t op ()
insert t k v = singleton $ Insert t k v

-- | Delete a value for the given key in the given column
delete
    :: (GCompare t, Ord (KeyOf c))
    => t c
    -- ^ column
    -> KeyOf c
    -- ^ key
    -> Transaction m cf t op ()
delete t k = singleton $ Delete t k

interpretQuery
    :: (GCompare t, Ord (KeyOf f), MonadFail m)
    => t f
    -> KeyOf f
    -> Context m cf t op (Maybe (ValueOf f))
interpretQuery t k = Context $ do
    workspaces <- get
    case DMap.lookup t workspaces of
        Just (Workspace ws) -> maybe fetchFromDB pure $ Map.lookup k ws
        Nothing -> fetchFromDB
  where
    fetchFromDB = do
        Database{valueAt, columns} <- lift ask
        Column{family = cf, codecs = codecs} <-
            case DMap.lookup t columns of
                Just col -> pure col
                Nothing -> fail "query: column not found"
        rvalue <- lift $ lift $ valueAt cf $ review (keyCodec codecs) k
        mapM (decodeValueThrow codecs) rvalue

interpretInsert
    :: (GCompare t, Ord (KeyOf c), Monad m)
    => t c
    -> KeyOf c
    -> ValueOf c
    -> Context m cf t op ()
interpretInsert t k v =
    Context
        $ modify
        $ DMap.adjust (overWorkspace (Map.insert k (Just v))) t

interpretDelete
    :: (GCompare t, Ord (KeyOf c), Monad m)
    => t c
    -> KeyOf c
    -> Context m cf t op ()
interpretDelete t k =
    Context
        $ modify
        $ DMap.adjust (overWorkspace (Map.insert k Nothing)) t

-- | Interpret the transaction as a value in the Context monad
interpretTransaction
    :: (GCompare t, MonadFail m)
    => Transaction m cf t op a
    -> Context m cf t op a
interpretTransaction prog = do
    v <- viewT prog
    case v of
        Return a -> pure a
        instr :>>= k -> case instr of
            Query t key -> do
                r <- interpretQuery t key
                interpretTransaction (k r)
            Insert t key value -> do
                interpretInsert t key value
                interpretTransaction (k ())
            Delete t key -> do
                interpretDelete t key
                interpretTransaction (k ())

-- | Run a transaction in the given database context
run
    :: forall m t cf op b
     . (GCompare t, MonadFail m)
    => Database m cf t op
    -> Transaction m cf t op b
    -> m b
run db@Database{columns, applyOps} tx = do
    let emptyWorkspaces = DMap.map (const (Workspace Map.empty)) columns
    (result, workspaces) <-
        runReaderT
            (runStateT (unContext $ interpretTransaction tx) emptyWorkspaces)
            db
    ops <- mapM toBatchOps $ DMap.toList workspaces
    applyOps $ concat ops
    pure result
  where
    toBatchOps :: DSum t Workspace -> m [op]
    toBatchOps (sel :=> Workspace ws) =
        case DMap.lookup sel columns of
            Just column -> pure $ uncurry (mkOp db column) <$> Map.toList ws
            Nothing -> fail "runTransaction: column not found"

mkOp
    :: Database m cf t op
    -> Column cf c
    -> KeyOf c
    -> Maybe (ValueOf c)
    -> op
mkOp
    Database{mkOperation}
    Column{family, codecs = Codecs{keyCodec, valueCodec}}
    k = mkOperation family (review keyCodec k) . fmap (review valueCodec)

mkCols :: GCompare t => [DSum t r] -> DMap t r
mkCols = DMap.fromList
