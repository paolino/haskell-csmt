{-# LANGUAGE StrictData #-}

module CSMT.Insertion
    ( inserting
    , mkCompose
    , scanCompose
    , Compose (..)
    )
where

import CSMT.Interface
    ( Direction (..)
    , FromKV (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , compareKeys
    , opposite
    )
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , insert
    , query
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

-- | Insert a key-value pair into the CSMT structure
inserting
    :: (Monad m, Ord k, GCompare d)
    => FromKV k v a
    -> Hashing a
    -> Selector d k v
    -> Selector d Key (Indirect a)
    -> k
    -> v
    -> Transaction m cf d ops ()
inserting FromKV{fromK, fromV} hashing kVCol csmtCol k v = do
    insert kVCol k v
    c <- mkCompose csmtCol (fromK k) (fromV v)
    mapM_ (uncurry $ insert csmtCol) $ snd $ scanCompose hashing c

-- Scan a Compose tree and produce the resulting hash and list of inserts
scanCompose
    :: Hashing a -> Compose a -> (Indirect a, [(Key, Indirect a)])
scanCompose Hashing{combineHash} = go []
  where
    go k (Leaf i) = (i, [(k, i)])
    go k (Compose jump left right) =
        let k' = k <> jump
            (hl, ls) = go (k' <> [L]) left
            (hr, rs) = go (k' <> [R]) right
            value = combineHash hl hr
            i = Indirect{jump, value}
        in  (i, ls <> rs <> [(k, i)])

-- Build a Compose tree for inserting a value at a given key
mkCompose
    :: forall a d ops cf m
     . (Monad m, GCompare d)
    => Selector d Key (Indirect a)
    -> Key
    -> a
    -> Transaction m cf d ops (Compose a)
mkCompose csmtCol key h = go key [] pure
  where
    go [] _ cont = cont $ Leaf $ Indirect [] h
    go target current cont = do
        mi <- query csmtCol current
        case mi of
            Nothing -> cont $ Leaf $ Indirect target h
            Just Indirect{jump, value} -> do
                let (common, other, us) = compareKeys jump target
                case (other, us) of
                    ([], []) -> cont $ Leaf $ Indirect common h
                    ([], z : zs) -> do
                        mov <- query csmtCol (current <> common <> [opposite z])
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
