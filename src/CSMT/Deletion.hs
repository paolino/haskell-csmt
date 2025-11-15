module CSMT.Deletion
    ( deleting
    , newDeletionPath
    , DeletionPath (..)
    , deletionPathToOps
    )
where

import CSMT.Interface
    ( CSMT (..)
    , Direction (..)
    , Indirect (..)
    , Key
    , Op (..)
    , Query
    , compareKeys
    , opposite
    )
import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT (..))

data DeletionPath a where
    Value :: Key -> a -> DeletionPath a
    Branch
        :: Key -> Direction -> DeletionPath a -> Indirect a -> DeletionPath a

deriving instance Show a => Show (DeletionPath a)
deriving instance Eq a => Eq (DeletionPath a)

addWithDirection :: (a -> a -> a) -> Direction -> a -> a -> a
addWithDirection add L left right = add left right
addWithDirection add R left right = add right left

deleting :: Monad m => CSMT m a -> (a -> a -> a) -> Key -> m ()
deleting csmt add key = do
    mpath <- newDeletionPath (query csmt) key
    case mpath of
        Nothing -> pure ()
        Just path -> change csmt $ deletionPathToOps add path

deletionPathToOps
    :: (a -> a -> a)
    -> DeletionPath a
    -> [Op a]
deletionPathToOps add = snd . go []
  where
    go k (Value _ _v) = (Nothing, [Delete k])
    go k (Branch j d v i) =
        let
            (msb, xs) = go (k <> j <> [d]) v
        in
            case msb of
                Just v' ->
                    let h = addWithDirection add d v' (value i)
                    in  ( Just h
                        , xs <> [Insert k $ Indirect{jump = j, value = h}]
                        )
                Nothing ->
                    ( Just (value i)
                    , xs
                        <> [ Insert k
                                $ Indirect{jump = j <> jump i <> [opposite d], value = value i}
                           , Delete (k <> j <> [opposite d])
                           ]
                    )

newDeletionPath
    :: forall m a
     . Monad m
    => Query m a
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
