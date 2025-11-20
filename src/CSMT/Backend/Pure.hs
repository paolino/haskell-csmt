{-# LANGUAGE StrictData #-}

module CSMT.Backend.Pure
    ( InMemoryDB
    , Pure
    , runPure
    , pureCSMT
    )
where

import CSMT.Interface
    ( CSMT (..)
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
type InMemoryDB a = Map Key (Indirect a)

-- | Pure monad for CSMT operations
type Pure a = State (InMemoryDB a)

runPure :: InMemoryDB a -> State (InMemoryDB a) b -> (b, InMemoryDB a)
runPure = flip runState

pureChange :: InMemoryDB a -> Op k v a -> InMemoryDB a
pureChange m (InsertCSMT k v) = Map.insert k v m
pureChange m (DeleteCSMT k) = Map.delete k m
pureChange m (InsertKV _ _) = m -- Placeholder for KV insert
pureChange m (DeleteKV _) = m -- Placeholder for KV delete

pureCSMT :: CSMT (Pure a) k v a
pureCSMT =
    CSMT
        { change = \kvs -> modify' $ \m -> foldl' pureChange m kvs
        , query = gets . Map.lookup
        }
