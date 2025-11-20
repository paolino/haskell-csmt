module CSMT.Deletion
    ( deleting
    , newDeletionPath
    , DeletionPath (..)
    , deletionPathToOps
    )
where

import CSMT.Interface
    ( Backend (..)
    , Direction (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , Op (..)
    , QueryCSMT
    , addWithDirection
    , compareKeys
    , opposite
    )
import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT (..))

data DeletionPath a where
    Value :: Key -> a -> DeletionPath a
    Branch
        :: Key -> Direction -> DeletionPath a -> Indirect a -> DeletionPath a
    deriving (Show, Eq)

deleting
    :: Monad m => Backend m k v a -> Hashing a -> Key -> m ()
deleting csmt hashing key = do
    mpath <- newDeletionPath (queryCSMT csmt) key
    case mpath of
        Nothing -> pure ()
        Just path -> change csmt $ deletionPathToOps hashing path

deletionPathToOps
    :: forall k v a
     . Hashing a
    -> DeletionPath a
    -> [Op k v a]
deletionPathToOps hashing = snd . go []
  where
    go :: Key -> DeletionPath a -> (Maybe (Indirect a), [Op k v a])
    go k (Value _ _v) = (Nothing, [DeleteCSMT k])
    go k (Branch j d v i) =
        let
            (msb, xs) = go (k <> j <> [d]) v
        in
            case msb of
                Just i' ->
                    let h = addWithDirection hashing d i' i
                        i'' = Indirect{jump = j, value = h}
                    in  ( Just i''
                        , [InsertCSMT k i''] <> xs
                        )
                Nothing ->
                    let i' =
                            Indirect
                                { jump = j <> [opposite d] <> jump i
                                , value = value i
                                }
                    in  ( Just i'
                        , [ InsertCSMT k i'
                          , DeleteCSMT (k <> j <> [opposite d])
                          ]
                            <> xs
                        )

newDeletionPath
    :: forall m a
     . Monad m
    => QueryCSMT m a
    -> Key
    -> m (Maybe (DeletionPath a))
newDeletionPath q = runMaybeT . go []
  where
    go :: Key -> Key -> MaybeT m (DeletionPath a)
    go current remaining = do
        Indirect{jump = j, value = v} <- MaybeT $ q current
        let (_common, other, remaining') = compareKeys j remaining
        guard $ null other
        case remaining' of
            [] -> pure $ Value j v
            (r : remaining'') -> do
                let current' = current <> j
                sibiling <-
                    MaybeT $ q (current' <> [opposite r])
                p <- go (current' <> [r]) remaining''
                pure $ Branch j r p sibiling
