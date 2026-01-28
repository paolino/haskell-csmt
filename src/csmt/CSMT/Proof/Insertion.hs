{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Proof.Insertion
-- Description : Merkle inclusion proof generation and verification
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module provides functionality for generating and verifying Merkle
-- inclusion proofs. An inclusion proof demonstrates that a specific value
-- exists in the tree and contributes to the root hash.
--
-- A proof consists of a sequence of sibling hashes along the path from
-- the target value to the root, allowing verification without access to
-- the full tree.
module CSMT.Proof.Insertion
    ( InclusionProof (..)
    , ProofStep (..)
    , buildInclusionProof
    , verifyInclusionProof
    , computeRootHash
    )
where

import CSMT.Interface
    ( FromKV (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , addWithDirection
    , oppositeDirection
    )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.List (isPrefixOf)

import Control.Monad (guard)
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , query
    )

-- |
-- A single step in an inclusion proof.
--
-- Each step records:
-- * The number of key bits consumed at this step (direction + jump length)
-- * The sibling's indirect value (needed to recompute parent hash)
--
-- The direction and jump path are derived from the key during verification.
data ProofStep a = ProofStep
    { stepConsumed :: Int
    -- ^ Number of key bits consumed (1 for direction + jump length)
    , stepSibling :: Indirect a
    -- ^ Sibling indirect value
    }
    deriving (Show, Eq)

-- |
-- A self-contained inclusion proof for a key-value pair.
--
-- Contains all information needed to verify that a key-value pair
-- exists in a tree with a specific root hash. Can be serialized
-- and transmitted independently.
data InclusionProof a = InclusionProof
    { proofKey :: Key
    -- ^ The key being proven
    , proofValue :: a
    -- ^ The value at the key
    , proofRootHash :: a
    -- ^ The root hash this proof validates against
    , proofSteps :: [ProofStep a]
    -- ^ Steps from leaf to root
    , proofRootJump :: Key
    -- ^ Jump path at the root node
    }
    deriving (Show, Eq)

-- |
-- Generate an inclusion proof for a key-value pair in the CSMT.
--
-- Traverses from root to the target key, collecting sibling hashes at each
-- branch. Returns 'Nothing' if the key is not in the tree.
--
-- The returned proof is self-contained with the key, value, and root hash.
buildInclusionProof
    :: (Monad m, GCompare d)
    => FromKV k v a
    -> Selector d Key (Indirect a)
    -> Hashing a
    -> k
    -> v
    -> Transaction m cf d ops (Maybe (InclusionProof a))
buildInclusionProof FromKV{fromK, fromV} sel hashing k v = runMaybeT $ do
    let key = fromK k
        value = fromV v
    rootIndirect@(Indirect rootJump _) <- MaybeT $ query sel []
    guard $ isPrefixOf rootJump key
    steps <- go rootJump $ drop (length rootJump) key
    let proofData =
            InclusionProof
                { proofKey = key
                , proofValue = value
                , proofRootHash = rootHash hashing rootIndirect
                , proofSteps = reverse steps
                , proofRootJump = rootJump
                }
    pure proofData
  where
    go _ [] = pure []
    go u (x : ks) = do
        Indirect jump _ <- MaybeT $ query sel (u <> [x])
        guard $ isPrefixOf jump ks
        stepSibling <- MaybeT $ query sel (u <> [oppositeDirection x])
        let step =
                ProofStep
                    { stepConsumed = 1 + length jump
                    , stepSibling
                    }
        (step :)
            <$> go
                (u <> (x : jump))
                (drop (length jump) ks)

-- |
-- Verify an inclusion proof is internally consistent.
--
-- Recomputes the root hash from the proof data and checks it matches
-- the claimed root hash. This is a pure function that requires no
-- database access.
--
-- To verify against a trusted root, compare 'proofRootHash' with
-- your trusted value after this returns 'True'.
verifyInclusionProof :: Eq a => Hashing a -> InclusionProof a -> Bool
verifyInclusionProof hashing proof =
    proofRootHash proof == computeRootHash hashing proof

-- |
-- Compute the root hash from an inclusion proof.
--
-- Recomputes the Merkle root by combining the proof value with
-- sibling hashes along the path.
computeRootHash :: Hashing a -> InclusionProof a -> a
computeRootHash hashing InclusionProof{proofKey, proofValue, proofSteps, proofRootJump} =
    rootHash hashing (Indirect proofRootJump rootValue)
  where
    keyAfterRoot = drop (length proofRootJump) proofKey
    -- Reverse key so we can consume from the leaf end first
    rootValue = go proofValue (reverse keyAfterRoot) proofSteps

    go acc _ [] = acc
    go acc revKey (ProofStep{stepConsumed, stepSibling} : rest) =
        let (consumedRev, remainingRev) = splitAt stepConsumed revKey
            consumed = reverse consumedRev
        in  case consumed of
                (direction : stepJump) ->
                    go
                        ( addWithDirection
                            hashing
                            direction
                            (Indirect stepJump acc)
                            stepSibling
                        )
                        remainingRev
                        rest
                [] ->
                    -- Invalid proof: stepConsumed is 0 which shouldn't happen
                    error "computeRootHash: invalid proof step with zero consumed bits"
