{-# LANGUAGE StrictData #-}

module CSMT.Insertion
    ( inserting
    , mkCompose
    , scanCompose
    , Compose (..)
    )
where

import CSMT.Interface
    ( CSMT (..)
    , Direction (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , Op (..)
    , Query
    , compareKeys
    , opposite
    )

-- A binary tree with a Key at each node
data Compose a
    = Compose Key (Compose a) (Compose a)
    | Leaf (Indirect a)
    deriving (Show, Eq)

-- Construct a Compose node composed of two subtrees
compose :: Direction -> Key -> Compose a -> Compose a -> Compose a
compose L j left right = Compose j left right
compose R j left right = Compose j right left

-- | Change a value into a CSMT
inserting
    :: Monad m
    => CSMT m k v a
    -- ^ Backend interface of the CSMT
    -> Hashing a
    -> Key
    -- ^ Key to insert at
    -> a
    -- ^ Hash to insert
    -> m ()
inserting (CSMT i q) hashing key value = do
    c <- mkCompose q key value
    i $ snd $ scanCompose hashing c

-- Scan a Compose tree and produce the resulting hash and list of inserts
scanCompose
    :: Hashing a -> Compose a -> (Indirect a, [Op k v a])
scanCompose Hashing{combineHash} = go []
  where
    go k (Leaf i) = (i, [InsertCSMT k i])
    go k (Compose jump left right) =
        let k' = k <> jump
            (hl, ls) = go (k' <> [L]) left
            (hr, rs) = go (k' <> [R]) right
            value = combineHash hl hr
            i = Indirect{jump, value}
        in  (i, ls <> rs <> [InsertCSMT k i])

-- Build a Compose tree for inserting a value at a given key
mkCompose
    :: forall a m
     . Monad m
    => Query m a
    -> Key
    -> a
    -> m (Compose a)
mkCompose get key h = go key [] pure
  where
    go :: Key -> Key -> (Compose a -> m (Compose a)) -> m (Compose a)
    go [] _ cont = cont $ Leaf $ Indirect [] h
    go target current cont = do
        mi <- get current
        case mi of
            Nothing -> cont $ Leaf $ Indirect target h
            Just Indirect{jump, value} -> do
                let (common, other, us) = compareKeys jump target
                case (other, us) of
                    ([], []) -> cont $ Leaf $ Indirect common h
                    ([], z : zs) -> do
                        mov <- get (current <> common <> [opposite z])
                        case mov of
                            Nothing -> error "a jump pointed to a non-existing node"
                            Just i ->
                                go zs (current <> common <> [z]) $ \c ->
                                    cont $ compose z common c $ Leaf i
                    (_ : os, z : zs) ->
                        go zs (current <> common <> [z]) $ \c ->
                            cont $ compose z common c $ Leaf $ Indirect{jump = os, value}
                    _ ->
                        error
                            "there is at least on key longer than the requested key to insert"
