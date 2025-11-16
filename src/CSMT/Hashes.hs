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
    )
where

import CSMT.Insertion (inserting)
import CSMT.Interface (CSMT (..), Direction (..), Key)
import CSMT.Interface qualified as Interface
import CSMT.Proofs qualified as Proof
import Crypto.Hash (Blake2b_256, hash)
import Data.Bits (Bits (..))
import Data.ByteArray (ByteArray, ByteArrayAccess, convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Word (Word8)

newtype Hash = Hash ByteString
    deriving
        (Eq, Ord, Show, Semigroup, Monoid, ByteArrayAccess, ByteArray)

mkHash :: ByteString -> Hash
mkHash = convert . hash @ByteString @Blake2b_256

addHash :: Hash -> Hash -> Hash
addHash (Hash h1) (Hash h2) = mkHash (h1 <> h2)

renderHash :: Hash -> ByteString
renderHash (Hash h) = h

parseHash :: ByteString -> Maybe Hash
parseHash bs
    | B.length bs == 32 = Just (Hash bs)
    | otherwise = Nothing

insert :: Monad m => CSMT m Hash -> ByteString -> ByteString -> m ()
insert csmt k v = inserting csmt addHash (byteStringToKey k) (mkHash v)

delete :: Monad m => CSMT m Hash -> ByteString -> m ()
delete csmt k = inserting csmt addHash (byteStringToKey k) mempty

byteStringToKey :: ByteString -> Key
byteStringToKey bs = concatMap byteToDirections (B.unpack $ renderHash $ mkHash bs)

byteToDirections :: Word8 -> Key
byteToDirections byte = [if testBit byte i then R else L | i <- [7, 6 .. 0]]

root :: Monad m => CSMT m Hash -> m (Maybe ByteString)
root csmt = do
    mi <- Interface.root csmt
    case mi of
        Nothing -> return Nothing
        Just v -> return (Just $ renderHash v)

renderProof :: Proof.Proof Hash -> ByteString
renderProof = B.concat . fmap renderStep
  where
    renderStep (dir, h) =
        let dirByte = case dir of
                L -> 0x00
                R -> 0x01
        in  B.cons dirByte (renderHash h)

parseProof :: ByteString -> Maybe (Proof.Proof Hash)
parseProof = go
  where
    go bs
        | B.null bs = Just []
        | B.length bs < 33 = Nothing
        | otherwise = do
            let (dirByte, rest) = B.splitAt 1 bs
                (hashBytes, rest') = B.splitAt 32 rest
            dir <- case B.head dirByte of
                0x00 -> Just L
                0x01 -> Just R
                _ -> Nothing
            h <- parseHash hashBytes
            steps <- go rest'
            Just ((dir, h) : steps)

generateInclusionProof
    :: Monad m => CSMT m Hash -> ByteString -> m (Maybe ByteString)
generateInclusionProof csmt k = do
    mp <- Proof.mkInclusionProof csmt (byteStringToKey k)
    pure $ fmap renderProof mp

verifyInclusionProof
    :: Monad m
    => CSMT m Hash
    -> ByteString
    -> ByteString
    -> m Bool
verifyInclusionProof csmt value proofBs = do
    case parseProof proofBs of
        Nothing -> pure False
        Just proof -> do
            let valueHash = mkHash value
            Proof.verifyInclusionProof csmt addHash valueHash proof
