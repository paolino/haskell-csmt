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
    , Indirect
    , Key
    , getIndirect
    , getKey
    , getSizedByteString
    , putIndirect
    , putKey
    , putSizedByteString
    )
import CSMT.Interface qualified as Interface
import CSMT.Proof.Insertion (InclusionProof (..), ProofStep (..))
import CSMT.Proof.Insertion qualified as Proof
import Control.Lens (Iso', iso)
import Control.Monad (forM_, replicateM)
import Crypto.Hash (Blake2b_256, hash)
import Data.Bits (Bits (..))
import Data.ByteArray (ByteArray, ByteArrayAccess, convert)
import Data.ByteArray.Encoding (Base (Base64), convertToBase)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Serialize
    ( Get
    , PutM
    , getWord16be
    , putWord16be
    , runGet
    )
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

-- | Serialize a proof to binary format.
putProof :: InclusionProof Hash -> PutM ()
putProof pf = do
    putKey $ proofKey pf
    putSizedByteString $ proofValue pf
    putSizedByteString $ proofRootHash pf
    putKey $ proofRootJump pf
    putWord16be (fromIntegral $ length $ proofSteps pf)
    forM_ (proofSteps pf) $ \ProofStep{stepConsumed, stepSibling} -> do
        putWord16be (fromIntegral stepConsumed)
        putIndirect stepSibling

-- | Render a proof to a ByteString.
renderProof :: InclusionProof Hash -> ByteString
renderProof pf = evalPutM $ putProof pf

-- | Deserialize a proof from binary format.
getProof :: Get (InclusionProof Hash)
getProof = do
    proofKey <- getKey
    proofValue <- getSizedByteString
    proofRootHash <- getSizedByteString
    proofRootJump <- getKey
    len <- getWord16be
    proofSteps <- replicateM
        (fromIntegral len)
        $ do
            stepConsumed <- fromIntegral <$> getWord16be
            stepSibling <- getIndirect
            return $ ProofStep{stepConsumed, stepSibling}
    return
        $ InclusionProof
            { proofKey
            , proofValue
            , proofRootHash
            , proofSteps
            , proofRootJump
            }

-- | Parse a ByteString as a proof. Returns Nothing on parse failure.
parseProof :: ByteString -> Maybe (InclusionProof Hash)
parseProof bs =
    case runGet getProof bs of
        Left _ -> Nothing
        Right pf -> Just pf

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
