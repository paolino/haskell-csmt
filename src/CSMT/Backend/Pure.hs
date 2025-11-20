{-# LANGUAGE StrictData #-}

module CSMT.Backend.Pure
    ( InMemoryDB (..)
    , Pure
    , runPure
    , pureBackend
    , emptyInMemoryDB
    )
where

import CSMT.Interface
    ( Backend (..)
    , Indirect
    , Key
    , Op (..)
    )
import Control.Monad.Trans.State.Strict
    ( State
    , gets
    , modify'
    , runState
    )
import Data.Foldable (Foldable (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | In-memory database type from keys to indirect values
data InMemoryDB k v a = InMemoryDB
    { inMemoryCSMT :: Map Key (Indirect a)
    , inMemoryKV :: Map k v
    }
    deriving (Show, Eq)

emptyInMemoryDB :: InMemoryDB k v a
emptyInMemoryDB = InMemoryDB Map.empty Map.empty

onCSMT
    :: InMemoryDB k v a
    -> (Map Key (Indirect a) -> Map Key (Indirect a))
    -> InMemoryDB k v a
onCSMT m f = m{inMemoryCSMT = f (inMemoryCSMT m)}

onKV :: InMemoryDB k v a -> (Map k v -> Map k v) -> InMemoryDB k v a
onKV m f = m{inMemoryKV = f (inMemoryKV m)}

-- | Pure monad for CSMT operations
type Pure k v a = State (InMemoryDB k v a)

runPure
    :: InMemoryDB k v a
    -> State (InMemoryDB k v a) b
    -> (b, InMemoryDB k v a)
runPure = flip runState

pureChange
    :: Ord k => InMemoryDB k v a -> Op k v a -> InMemoryDB k v a
pureChange m (InsertCSMT k v) = onCSMT m $ Map.insert k v
pureChange m (DeleteCSMT k) = onCSMT m $ Map.delete k
pureChange m (InsertKV k v) = onKV m (Map.insert k v)
pureChange m (DeleteKV k) = onKV m (Map.delete k)

pureBackend :: Ord k => Backend (Pure k v a) k v a
pureBackend =
    Backend
        { change = \kvs -> modify' $ \m -> foldl' pureChange m kvs
        , queryCSMT = \k -> gets $ Map.lookup k . inMemoryCSMT
        , queryKV = \k -> gets $ Map.lookup k . inMemoryKV
        }
