module CSMT.Deletion
    ( deleting
    , newDeletionPath
    , DeletionPath (..)
    , deletionPathToOps
    )
where

import CSMT.Interface
    ( Direction (..)
    , FromKV (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , addWithDirection
    , compareKeys
    , opposite
    )
import Control.Monad (guard)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , delete
    , insert
    , query
    )

data DeletionPath a where
    Value :: Key -> a -> DeletionPath a
    Branch
        :: Key -> Direction -> DeletionPath a -> Indirect a -> DeletionPath a
    deriving (Show, Eq)

deleting
    :: (Monad m, Ord k, GCompare d)
    => FromKV k v a
    -> Hashing a
    -> Selector d k v
    -> Selector d Key (Indirect a)
    -> k
    -> Transaction m cf d ops ()
deleting FromKV{fromK} hashing kvSel csmtSel key = do
    mpath <- newDeletionPath csmtSel (fromK key)
    case mpath of
        Nothing -> pure ()
        Just path -> do
            delete kvSel key
            mapM_ (applyOp csmtSel) $ deletionPathToOps hashing path
applyOp
    :: GCompare d
    => Selector d Key (Indirect a)
    -> (Key, Maybe (Indirect a))
    -> Transaction m cf d ops ()
applyOp csmtSel (k, Nothing) = delete csmtSel k
applyOp csmtSel (k, Just i) = insert csmtSel k i

deletionPathToOps
    :: forall a
     . Hashing a
    -> DeletionPath a
    -> [(Key, Maybe (Indirect a))]
deletionPathToOps hashing = snd . go []
  where
    go
        :: Key
        -> DeletionPath a
        -> (Maybe (Indirect a), [(Key, Maybe (Indirect a))])
    go k (Value _ _v) = (Nothing, [(k, Nothing)])
    go k (Branch j d v i) =
        let
            (msb, xs) = go (k <> j <> [d]) v
        in
            case msb of
                Just i' ->
                    let h = addWithDirection hashing d i' i
                        i'' = Indirect{jump = j, value = h}
                    in  ( Just i''
                        , [(k, Just i'')] <> xs
                        )
                Nothing ->
                    let i' =
                            Indirect
                                { jump = j <> [opposite d] <> jump i
                                , value = value i
                                }
                    in  ( Just i'
                        , [ (k, Just i')
                          , (k <> j <> [opposite d], Nothing)
                          ]
                            <> xs
                        )

newDeletionPath
    :: forall a d ops cf m
     . (Monad m, GCompare d)
    => Selector d Key (Indirect a)
    -> Key
    -> Transaction m cf d ops (Maybe (DeletionPath a))
newDeletionPath csmtSel = runMaybeT . go []
  where
    go
        :: Key
        -> Key
        -> MaybeT (Transaction m cf d ops) (DeletionPath a)
    go current remaining = do
        Indirect{jump = j, value = v} <- MaybeT $ query csmtSel current
        let (_common, other, remaining') = compareKeys j remaining
        guard $ null other
        case remaining' of
            [] -> pure $ Value j v
            (r : remaining'') -> do
                let current' = current <> j
                sibiling <-
                    MaybeT $ query csmtSel (current' <> [opposite r])
                p <- go (current' <> [r]) remaining''
                pure $ Branch j r p sibiling
