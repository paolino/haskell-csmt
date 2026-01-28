{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Hashes
-- Description : Blake2b-256 based hashing for CSMT
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module provides a concrete hash implementation for CSMTs using
-- Blake2b-256. It includes:
--
-- * 'Hash' - A 32-byte hash value
-- * 'hashHashing' - Hashing functions for tree operations
-- * Serialization functions for proofs and hashes
-- * High-level API for insert, delete, and proof operations
module CSMT.Hashes
    ( mkHash
    , addHash
    , Hash (..)
    , renderHash
    , parseHash
    , insert
    , root
    , generateInclusionProof
    , verifyInclusionProof
    , renderProof
    , parseProof
    , delete
    , hashHashing
    , keyToHash
    , byteStringToKey
    , isoHash
    , fromKVHashes
    )
where

import CSMT.Deletion (deleting)
import CSMT.Insertion (inserting)
import CSMT.Interface
    ( Direction (..)
    , FromKV (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , putIndirect
    , putKey
    )
import CSMT.Interface qualified as Interface
import CSMT.Proof.Insertion (InclusionProof (..), ProofStep (..))
import CSMT.Proof.Insertion qualified as Proof
import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Lens (Iso', iso)
import Control.Monad (replicateM)
import Crypto.Hash (Blake2b_256, hash)
import Data.Bits (Bits (..))
import Data.ByteArray (ByteArray, ByteArrayAccess, convert)
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Serialize.Extra (evalPutM)
import Data.Word (Word8)
import Database.KV.Transaction (GCompare, Selector, Transaction)

-- | A 32-byte Blake2b-256 hash value.
newtype Hash = Hash ByteString
    deriving
        (Eq, Ord, Semigroup, Monoid, ByteArrayAccess, ByteArray)

instance Show Hash where
    show (Hash h) = BC.unpack $ "Hash " <> convertToBase Base64 h

-- | Compute a Blake2b-256 hash of a ByteString.
mkHash :: ByteString -> Hash
mkHash = convert . hash @ByteString @Blake2b_256

-- | Hashing functions for building CSMT with Blake2b-256.
hashHashing :: Hashing Hash
hashHashing =
    Hashing
        { rootHash = mkHash . evalPutM . putIndirect
        , combineHash = \left right -> mkHash . evalPutM $ do
            putIndirect left
            putIndirect right
        }

-- | Combine two hashes by concatenating and rehashing.
addHash :: Hash -> Hash -> Hash
addHash (Hash h1) (Hash h2) = mkHash (h1 <> h2)

-- | Extract the raw ByteString from a Hash.
renderHash :: Hash -> ByteString
renderHash (Hash h) = h

-- | Parse a 32-byte ByteString as a Hash. Returns Nothing if length is wrong.
parseHash :: ByteString -> Maybe Hash
parseHash bs
    | B.length bs == 32 = Just (Hash bs)
    | otherwise = Nothing

-- | Convert a Key to its hash representation.
keyToHash :: Key -> Hash
keyToHash = mkHash . evalPutM . putKey

-- | Insert a key-value pair using Blake2b-256 hashing.
insert
    :: (Monad m, Ord k, GCompare d)
    => FromKV k v Hash
    -> Selector d k v
    -> Selector d Key (Indirect Hash)
    -> k
    -> v
    -> Transaction m cf d ops ()
insert csmt = inserting csmt hashHashing

-- | Delete a key-value pair using Blake2b-256 hashing.
delete
    :: (Monad m, Ord k, GCompare d)
    => FromKV k v Hash
    -> Selector d k v
    -> Selector d Key (Indirect Hash)
    -> k
    -> Transaction m cf d ops ()
delete csmt = deleting csmt hashHashing

-- | Convert a ByteString to a Key by expanding each byte to 8 directions.
byteStringToKey :: ByteString -> Key
byteStringToKey bs = concatMap byteToDirections (B.unpack bs)

-- | Convert a byte to 8 directions (one per bit, MSB first).
byteToDirections :: Word8 -> Key
byteToDirections byte = [if testBit byte i then R else L | i <- [7, 6 .. 0]]

-- | Get the root hash of the tree, if it exists.
root
    :: (Monad m, GCompare d)
    => Selector d Key (Indirect Hash)
    -> Transaction m cf d ops (Maybe ByteString)
root csmt = do
    mi <- Interface.root hashHashing csmt
    case mi of
        Nothing -> return Nothing
        Just v -> return (Just $ renderHash v)

-- | Encode a Direction to CBOR (L = 0, R = 1).
encodeDirection :: Direction -> CBOR.Encoding
encodeDirection L = CBOR.encodeWord 0
encodeDirection R = CBOR.encodeWord 1

-- | Decode a Direction from CBOR.
decodeDirection :: CBOR.Decoder s Direction
decodeDirection = do
    w <- CBOR.decodeWord
    case w of
        0 -> pure L
        1 -> pure R
        _ -> fail "Invalid direction"

-- | Encode a Key (list of Directions) to CBOR.
encodeKey :: Key -> CBOR.Encoding
encodeKey dirs =
    CBOR.encodeListLen (fromIntegral $ length dirs)
        <> foldMap encodeDirection dirs

-- | Decode a Key from CBOR.
decodeKey :: CBOR.Decoder s Key
decodeKey = do
    len <- CBOR.decodeListLen
    replicateM len decodeDirection

-- | Encode an Indirect to CBOR.
encodeIndirect :: Indirect Hash -> CBOR.Encoding
encodeIndirect Indirect{jump, value} =
    CBOR.encodeListLen 2
        <> encodeKey jump
        <> CBOR.encodeBytes (renderHash value)

-- | Decode an Indirect from CBOR.
decodeIndirect :: CBOR.Decoder s (Indirect Hash)
decodeIndirect = do
    _ <- CBOR.decodeListLen
    jump <- decodeKey
    value <- Hash <$> CBOR.decodeBytes
    pure Indirect{jump, value}

-- | Encode a ProofStep to CBOR.
encodeProofStep :: ProofStep Hash -> CBOR.Encoding
encodeProofStep ProofStep{stepConsumed, stepSibling} =
    CBOR.encodeListLen 2
        <> CBOR.encodeInt stepConsumed
        <> encodeIndirect stepSibling

-- | Decode a ProofStep from CBOR.
decodeProofStep :: CBOR.Decoder s (ProofStep Hash)
decodeProofStep = do
    _ <- CBOR.decodeListLen
    stepConsumed <- CBOR.decodeInt
    stepSibling <- decodeIndirect
    pure ProofStep{stepConsumed, stepSibling}

-- | Encode an InclusionProof to CBOR.
encodeProof :: InclusionProof Hash -> CBOR.Encoding
encodeProof InclusionProof{proofKey, proofValue, proofRootHash, proofSteps, proofRootJump} =
    CBOR.encodeListLen 5
        <> encodeKey proofKey
        <> CBOR.encodeBytes (renderHash proofValue)
        <> CBOR.encodeBytes (renderHash proofRootHash)
        <> ( CBOR.encodeListLen (fromIntegral $ length proofSteps)
                <> foldMap encodeProofStep proofSteps
           )
        <> encodeKey proofRootJump

-- | Decode an InclusionProof from CBOR.
decodeProof :: CBOR.Decoder s (InclusionProof Hash)
decodeProof = do
    _ <- CBOR.decodeListLen
    proofKey <- decodeKey
    proofValue <- Hash <$> CBOR.decodeBytes
    proofRootHash <- Hash <$> CBOR.decodeBytes
    stepsLen <- CBOR.decodeListLen
    proofSteps <- replicateM stepsLen decodeProofStep
    proofRootJump <- decodeKey
    pure
        InclusionProof
            { proofKey
            , proofValue
            , proofRootHash
            , proofSteps
            , proofRootJump
            }

-- | Render a proof to a ByteString using CBOR.
renderProof :: InclusionProof Hash -> ByteString
renderProof = BL.toStrict . CBOR.toLazyByteString . encodeProof

-- | Parse a ByteString as a proof. Returns Nothing on parse failure.
parseProof :: ByteString -> Maybe (InclusionProof Hash)
parseProof bs =
    case CBOR.deserialiseFromBytes decodeProof (BL.fromStrict bs) of
        Left _ -> Nothing
        Right (_, pf) -> Just pf

-- | Generate an inclusion proof for a key-value pair.
-- Returns the serialized proof.
generateInclusionProof
    :: (Monad m, GCompare d)
    => FromKV k v Hash
    -> Selector d Key (Indirect Hash)
    -> k
    -> v
    -> Transaction m cf d ops (Maybe ByteString)
generateInclusionProof csmt sel k v = do
    mp <- Proof.buildInclusionProof csmt sel hashHashing k v
    pure $ fmap renderProof mp

-- | Verify an inclusion proof from a serialized ByteString.
-- Returns True if the proof is internally consistent.
verifyInclusionProof :: ByteString -> Bool
verifyInclusionProof proofBs =
    case parseProof proofBs of
        Nothing -> False
        Just proof -> Proof.verifyInclusionProof hashHashing proof

-- | Isomorphism between ByteString and Hash.
isoHash :: Iso' ByteString Hash
isoHash = iso Hash renderHash

-- | Default FromKV for ByteString keys and values with Blake2b-256 hashing.
fromKVHashes :: FromKV ByteString ByteString Hash
fromKVHashes =
    FromKV
        { fromK = byteStringToKey
        , fromV = mkHash
        }
