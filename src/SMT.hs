module SMT
    ( Direction (..)
    , Key
    , Proof
    , Pure
    , inserting
    , mkProof
    , pureInsert
    , pureQuery
    , runPure
    , verifyProof
    )
where

import Control.Monad.State
    ( State
    , gets
    , modify'
    , runState
    )
import Data.Foldable (Foldable (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)

type Key = [Direction]

data Direction = L | R deriving (Show, Eq, Ord)

opposite :: Direction -> Direction
opposite L = R
opposite R = L

data Compose a
    = Compose (Compose a) (Compose a)
    | Leaf a
    deriving (Show, Eq)

compose :: Direction -> Compose a -> Compose a -> Compose a
compose L left right = Compose left right
compose R left right = Compose right left

type Insert m a = [(Key, a)] -> m ()
type Query m a = (a, Key -> m (Maybe a))

inserting
    :: Monad m
    => Query m a
    -> Insert m a
    -> (a -> a -> a)
    -> Key
    -> a
    -> m ()
inserting q i add key value = do
    c <- mkCompose q key value
    i
        $ snd
        $ scanCompose add c

scanCompose :: (a -> a -> a) -> Compose a -> (a, [(Key, a)])
scanCompose add = go []
  where
    go k (Leaf h) = (h, [(k, h)])
    go k (Compose left right) =
        let (hl, ls) = go (k ++ [L]) left
            (hr, rs) = go (k ++ [R]) right
            h = add hl hr
        in  (h, ls <> rs <> [(k, h)])

mkCompose
    :: forall a m. Monad m => Query m a -> Key -> a -> m (Compose a)
mkCompose (def, get) key h = go key [] pure
  where
    go :: Key -> Key -> (Compose a -> m (Compose a)) -> m (Compose a)
    go [] _ cont = cont $ Leaf h
    go (k : ks) u cont = go ks (u <> [k]) $ \c -> do
        l <- Leaf . fromMaybe def <$> get (u <> [opposite k])
        cont $ compose k c l

type Proof a = [(Direction, a)]

mkProof :: Monad m => Query m a -> Key -> m (Maybe (Proof a))
mkProof (def, find) key = go [] key []
  where
    go _ [] rs = pure $ Just rs
    go u (k : ks) rs = do
        mr <- find (u <> [k])
        case mr of
            Nothing -> pure Nothing
            Just _ -> do
                o <- fromMaybe def <$> find (u <> [opposite k])
                go
                    (u <> [k])
                    ks
                    ((k, o) : rs)

foldProof :: (a -> a -> a) -> a -> Proof a -> a
foldProof add = foldl' step
  where
    step acc (L, h) = add acc h
    step acc (R, h) = add h acc

verifyProof
    :: (Eq a, Monad m)
    => Query m a
    -> (a -> a -> a)
    -> a
    -> Proof a
    -> m Bool
verifyProof (_, find) add value proof = do
    mv <- find []
    pure $ case mv of
        Just rootHash -> rootHash == foldProof add value proof
        Nothing -> False

--------------- Pure as a Map in the State ---------------

type Pure a = State (Map Key a)

pureQuery :: a -> Query (Pure a) a
pureQuery def = (def, gets . Map.lookup)

pureInsert :: Insert (Pure a) a
pureInsert kvs = modify' $ \m -> Map.fromList kvs <> m

runPure :: Map Key a -> Pure a b -> (b, Map Key a)
runPure = flip runState

------------- Hash-based SMT --------------

-- type Hash = ByteString
-- type SMTMap = Map Key Hash

-- fromBool :: Bool -> Direction
-- fromBool True = L
-- fromBool False = R

-- mkHash :: ByteString -> Hash
-- mkHash = convert . hash @ByteString @SHA256

-- defaultHash :: Hash
-- defaultHash = mkHash ""

-- root :: SMTMap -> Maybe Hash
-- root = Map.lookup []

-- mkKey :: ByteString -> Key
-- mkKey bs =
--     concatMap byteToKey (ByteString.unpack $ mkHash bs)
--   where
--     byteToKey w = [0 .. 7] <&> fromBool . testBit w

-- composeHash :: Hash -> Hash -> Hash
-- composeHash left right = mkHash (left <> right)

-- insert :: ByteString -> ByteString -> SMTMap -> SMTMap
-- insert key value = inserting composeHash defaultHash (mkKey key) (mkHash value)