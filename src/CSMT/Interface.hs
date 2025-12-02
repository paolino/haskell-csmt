{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

module CSMT.Interface
    ( -- * Keys
      Direction (..)
    , Key
    , compareKeys
    , opposite

      -- * Interface Types
    , Indirect (..)
    , Hashing (..)
    , Op (..)
    , Change
    , QueryCSMT
    , Backend (..)
    , FromKV (..)

      -- * Serialization Helpers
    , fromBool
    , toBool
    , root
    , putKey
    , getKey
    , putIndirect
    , getIndirect
    , putDirection
    , getDirection
    , getSizedByteString
    , putSizedByteString
    , addWithDirection
    , prefix
    )
where

import Data.Bits (Bits (..))
import Data.ByteArray (convert)
import Data.ByteArray qualified as BA
import Data.List (unfoldr)
import Data.Serialize
    ( Get
    , PutM
    , getByteString
    , getWord16be
    , getWord8
    , putByteString
    , putWord16be
    , putWord8
    )

-- | Key segment
data Direction = L | R deriving (Show, Eq, Ord)

-- | Convert Bool to Direction
fromBool :: Bool -> Direction
fromBool True = R
fromBool False = L

-- Convert Direction to its Bool representation
toBool :: Direction -> Bool
toBool L = False
toBool R = True

-- | Get the opposite direction
opposite :: Direction -> Direction
opposite L = R
opposite R = L

-- | Key type
type Key = [Direction]

-- | An indirect reference to a value stored at a given Key from a node
-- If the 'jump' key is empty then the value is stored at the current node
-- If the 'jump' key is non-empty then the value is stored at a descendant node
-- reachable by following the 'jump' key from the current node
data Indirect a = Indirect
    { jump :: Key
    , value :: a
    }
    deriving (Show, Eq, Functor, Ord)

prefix :: Key -> Indirect a -> Indirect a
prefix q Indirect{jump, value} = Indirect{jump = q ++ jump, value}

data Op k v a
    = InsertCSMT Key (Indirect a)
    | DeleteCSMT Key
    | InsertKV k v
    | DeleteKV k
    deriving (Show, Eq)

-- | Type alias for a change function in some monad m. It support batch inserts.
type Change m k v a = [Op k v a] -> m ()

-- | Type alias for a queryCSMT function in some monad m.
type QueryCSMT m a = Key -> m (Maybe (Indirect a))

type QueryKV m k v = k -> m (Maybe v)

-- | The backend interface for a CSMT in some monad m.
data Backend m k v a = Backend
    { change :: Change m k v a
    , queryCSMT :: QueryCSMT m a
    , queryKV :: QueryKV m k v
    , fromKV :: FromKV k v a
    }

data FromKV k v a
    = FromKV
    { fromK :: k -> Key
    , fromV :: v -> a
    }

-- | Compare two keys and return their common prefix and the remaining suffixes
-- of each key after the common prefix.
compareKeys :: Key -> Key -> (Key, Key, Key)
compareKeys [] ys = ([], [], ys)
compareKeys xs [] = ([], xs, [])
compareKeys (x : xs) (y : ys)
    | x == y =
        let (j, o, r) = compareKeys xs ys
        in  (x : j, o, r)
    | otherwise = ([], x : xs, y : ys)

root :: Monad m => Hashing a -> Backend m k v a -> m (Maybe a)
root hsh csmt = do
    mi <- queryCSMT csmt []
    pure $ case mi of
        Nothing -> Nothing
        Just i -> Just $ rootHash hsh i

bigendian :: [Int]
bigendian = [7, 6 .. 0]

putDirection :: Direction -> PutM ()
putDirection d = do
    putWord8 $ if toBool d then 1 else 0

getDirection :: Get Direction
getDirection = do
    b <- getWord8
    case b of
        0 -> return L
        1 -> return R
        _ -> fail "Invalid direction byte"

putKey :: Key -> PutM ()
putKey k = do
    let bytes = BA.pack $ unfoldr unconsDirection k
    putWord16be $ fromIntegral $ length k
    putByteString bytes
  where
    unconsDirection :: (Num a, Bits a) => Key -> Maybe (a, Key)
    unconsDirection [] = Nothing
    unconsDirection ds =
        let (byteBits, rest) = splitAt 8 ds
            byte = foldl setBitFromDir 0 (zip bigendian byteBits)
        in  Just (byte, rest)

    setBitFromDir :: Bits b => b -> (Int, Direction) -> b
    setBitFromDir b (i, dir)
        | toBool dir = setBit b i
        | otherwise = b

getKey :: Get Key
getKey = do
    len <- getWord16be
    let (l, r) = len `divMod` 8
        lr = if r == 0 then l else l + 1
    ba <- getByteString (fromIntegral lr)
    return
        $ take (fromIntegral len)
        $ concatMap byteToDirections (BA.unpack ba)
  where
    byteToDirections :: Bits b => b -> Key
    byteToDirections byte = [if testBit byte i then R else L | i <- bigendian]

putSizedByteString :: BA.ByteArrayAccess a => a -> PutM ()
putSizedByteString bs = do
    let len = fromIntegral $ BA.length bs
    putWord16be len
    putByteString $ convert bs

getSizedByteString :: BA.ByteArray a => Get a
getSizedByteString = do
    len <- getWord16be
    bs <- getByteString (fromIntegral len)
    return $ convert bs

-- | Serialize an Indirect to a ByteString
putIndirect
    :: BA.ByteArrayAccess a => Indirect a -> PutM ()
putIndirect Indirect{jump, value} = do
    putKey jump
    putSizedByteString value

-- | Deserialize a ByteString back to an Indirect
getIndirect :: BA.ByteArray a => Get (Indirect a)
getIndirect = Indirect <$> getKey <*> getSizedByteString

data Hashing a = Hashing
    { rootHash :: Indirect a -> a
    , combineHash :: Indirect a -> Indirect a -> a
    }

addWithDirection
    :: Hashing a -> Direction -> Indirect a -> Indirect a -> a
addWithDirection Hashing{combineHash} L left right = combineHash left right
addWithDirection Hashing{combineHash} R left right = combineHash right left
