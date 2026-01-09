{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

module CSMT.Interface
    ( -- * Keys
      Direction (..)
    , Key
    , keyPrism
    , compareKeys
    , opposite

      -- * Interface Types
    , Indirect (..)
    , Hashing (..)
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
    , csmtCodecs
    )
where

import Control.Lens (Prism', preview, prism', review, (<&>))
import Data.Bits (Bits (..))
import Data.ByteArray (convert)
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
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
import Data.Serialize.Extra (evalGetM, evalPutM, unsafeEvalGet)
import Database.KV.Transaction
    ( Codecs (..)
    , GCompare
    , KV
    , Selector
    , Transaction
    , query
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

root
    :: (Monad m, GCompare d)
    => Hashing a
    -> Selector d Key (Indirect a)
    -> Transaction m cf d ops (Maybe a)
root hsh sel = do
    mi <- query sel []
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

indirectPrism :: Prism' ByteString a -> Prism' ByteString (Indirect a)
indirectPrism prismA =
    prism'
        (evalPutM . putIndirect . fmap (review prismA))
        ( unsafeEvalGet $ do
            -- TODO: unsafe ?
            Indirect k x <- getIndirect
            pure $ preview prismA x <&> \a ->
                Indirect{jump = k, value = a}
        )

keyPrism :: Prism' ByteString Key
keyPrism = prism' (evalPutM . putKey) (evalGetM getKey)

csmtCodecs :: Prism' ByteString a -> Codecs (KV Key (Indirect a))
csmtCodecs prismA =
    Codecs
        { keyCodec = keyPrism
        , valueCodec = indirectPrism prismA
        }
