module CSMT.Backend.RocksDB.Key
    ( keyToRocksPathKey
    , rocksPathKeyToKey
    , RocksPathKey (..)
    , rocksValueToIndirect
    , indirectToRocksValue
    , rocksPathKeyToRocksKey
    , rocksKeyToRocksPathKey
    )
where

import CSMT.Interface (Indirect (..), Key, fromBool, toBool)
import Data.Bits (Bits (..), testBit)
import Data.ByteArray (convert)
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.List (unfoldr)
import Data.Serialize
    ( Get
    , PutM
    , getByteString
    , getWord16be
    , putByteString
    , putWord16be
    , runGet
    , runPutM
    )
import Data.Word (Word16, Word8)

data RocksPathKey = RocksPathKey
    { byteString :: ByteString
    , keyLength :: Word16
    }
    deriving (Eq, Show)

bigendian :: [Int]
bigendian = [7, 6 .. 0]

-- | Convert a CSMT key (list of Directions) to a RocksPathKey
keyToRocksPathKey :: Key -> RocksPathKey
keyToRocksPathKey directions =
    let keyLength = fromIntegral $ length directions
        byteString = BA.pack $ unfoldr buildBytes directions
    in  RocksPathKey
            { byteString
            , keyLength
            }
  where
    buildBytes [] = Nothing
    buildBytes ds =
        let (byteBits, rest) = splitAt 8 ds
            byte = foldl setBitFromDir 0 (zip bigendian byteBits)
        in  Just (byte, rest)
    setBitFromDir b (i, dir)
        | toBool dir = setBit b i
        | otherwise = b

-- | Convert a RocksPathKey back to a CSMT key (list of Directions)
rocksPathKeyToKey :: RocksPathKey -> Key
rocksPathKeyToKey RocksPathKey{byteString, keyLength} =
    let totalBits = fromIntegral keyLength
    in  fromBool
            <$> take totalBits (concatMap byteToBits (BA.unpack byteString))
  where
    byteToBits :: Word8 -> [Bool]
    byteToBits byte = [testBit byte i | i <- bigendian]

putRocksPathKey :: RocksPathKey -> PutM ()
putRocksPathKey RocksPathKey{byteString, keyLength} = do
    putWord16be keyLength
    putByteString byteString

getRocksPathKey :: Get RocksPathKey
getRocksPathKey = do
    keyLength <- getWord16be
    let (l, r) = keyLength `divMod` 8
        lr = if r == 0 then l else l + 1
    byteString <- getByteString (fromIntegral lr)
    return RocksPathKey{byteString, keyLength}

rocksPathKeyToRocksKey :: RocksPathKey -> ByteString
rocksPathKeyToRocksKey path =
    case runPutM (putRocksPathKey path) of
        ((), bs) -> bs

rocksKeyToRocksPathKey :: ByteString -> RocksPathKey
rocksKeyToRocksPathKey bs =
    case runGet getRocksPathKey bs of
        Right path -> path
        e -> error $ "rocksKeyToRocksPathKey: parse error" ++ show e

-- | Convert an Indirect to a RocksDB key (ByteString)
indirectToRocksValue
    :: BA.ByteArrayAccess a => Indirect a -> ByteString
indirectToRocksValue Indirect{jump, value} =
    let
        path = keyToRocksPathKey jump
        valueLength = BA.length value
        package = do
            putRocksPathKey path
            putWord16be $ fromIntegral valueLength
            putByteString $ convert value
    in
        case runPutM package of
            ((), bs) -> bs

-- | Convert a RocksDB key (ByteString) back to an Indirect
rocksValueToIndirect :: BA.ByteArray a => ByteString -> Indirect a
rocksValueToIndirect bs =
    let parser = do
            path <- getRocksPathKey
            valueLength <- getWord16be
            value <- getByteString (fromIntegral valueLength)
            return (path, value)
    in  case runGet parser bs of
            Right (path, value) ->
                Indirect
                    { jump = rocksPathKeyToKey path
                    , value = convert value
                    }
            e -> error $ "rocksValueToIndirect: parse error" ++ show e
