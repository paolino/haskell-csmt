{-# LANGUAGE StrictData #-}

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
    )
where

import CSMT.Deletion (deleting)
import CSMT.Insertion (inserting)
import CSMT.Interface
    ( CSMT (..)
    , Direction (..)
    , Hashing (..)
    , Key
    , getDirection
    , getIndirect
    , getKey
    , putDirection
    , putIndirect
    , putKey
    )
import CSMT.Interface qualified as Interface
import CSMT.Proofs (Proof (..), ProofStep (..))
import CSMT.Proofs qualified as Proof
import Control.Monad (forM_, replicateM)
import Crypto.Hash (Blake2b_256, hash)
import Data.Bits (Bits (..))
import Data.ByteArray (ByteArray, ByteArrayAccess, convert)
import Data.ByteArray.Encoding
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

newtype Hash = Hash ByteString
    deriving
        (Eq, Ord, Semigroup, Monoid, ByteArrayAccess, ByteArray)

instance Show Hash where
    show (Hash h) = BC.unpack $ "Hash " <> convertToBase Base64 h

mkHash :: ByteString -> Hash
mkHash = convert . hash @ByteString @Blake2b_256

hashHashing :: Hashing Hash
hashHashing =
    Hashing
        { rootHash = mkHash . evalPutM . putIndirect
        , combineHash = \left right -> mkHash . evalPutM $ do
            putIndirect left
            putIndirect right
        }

addHash :: Hash -> Hash -> Hash
addHash (Hash h1) (Hash h2) = mkHash (h1 <> h2)

renderHash :: Hash -> ByteString
renderHash (Hash h) = h

parseHash :: ByteString -> Maybe Hash
parseHash bs
    | B.length bs == 32 = Just (Hash bs)
    | otherwise = Nothing

keyToHash :: Key -> Hash
keyToHash = mkHash . evalPutM . putKey

insert
    :: Monad m => CSMT m k v Hash -> ByteString -> ByteString -> m ()
insert csmt k v = inserting csmt hashHashing (byteStringToKey k) (mkHash v)

delete :: Monad m => CSMT m k v Hash -> ByteString -> m ()
delete csmt k = deleting csmt hashHashing (byteStringToKey k)

byteStringToKey :: ByteString -> Key
byteStringToKey bs = concatMap byteToDirections (B.unpack $ renderHash $ mkHash bs)

byteToDirections :: Word8 -> Key
byteToDirections byte = [if testBit byte i then R else L | i <- [7, 6 .. 0]]

root :: Monad m => CSMT m k v Hash -> m (Maybe ByteString)
root csmt = do
    mi <- Interface.root hashHashing csmt
    case mi of
        Nothing -> return Nothing
        Just v -> return (Just $ renderHash v)

putProof :: Proof Hash -> PutM ()
putProof pf = do
    putKey $ proofRootJump pf
    putWord16be (fromIntegral $ length $ proofSteps pf)
    forM_ (proofSteps pf) $ \(ProofStep{stepDirection, stepSibiling, stepJump}) -> do
        putDirection stepDirection
        putIndirect stepSibiling
        putKey stepJump

renderProof :: Proof Hash -> ByteString
renderProof pf = evalPutM $ putProof pf

getProof :: Get (Proof Hash)
getProof = do
    proofRootJump <- getKey
    len <- getWord16be
    proofSteps <- replicateM
        (fromIntegral len)
        $ do
            stepDirection <- getDirection
            stepSibiling <- getIndirect
            stepJump <- getKey
            return $ ProofStep{stepDirection, stepSibiling, stepJump}
    return $ Proof{proofSteps, proofRootJump}

parseProof :: ByteString -> Maybe (Proof Hash)
parseProof bs =
    case runGet getProof bs of
        Left _ -> Nothing
        Right pf -> Just pf

generateInclusionProof
    :: Monad m => CSMT m k v Hash -> ByteString -> m (Maybe ByteString)
generateInclusionProof csmt k = do
    mp <- Proof.mkInclusionProof csmt (byteStringToKey k)
    pure $ fmap renderProof mp

verifyInclusionProof
    :: Monad m
    => CSMT m k v Hash
    -> ByteString
    -> ByteString
    -> m Bool
verifyInclusionProof csmt value proofBs = do
    case parseProof proofBs of
        Nothing -> pure False
        Just proof -> do
            let valueHash = mkHash value
            Proof.verifyInclusionProof csmt hashHashing valueHash proof
