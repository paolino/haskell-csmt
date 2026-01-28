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
    ( Proof (..)
    , ProofStep (..)
    , buildInclusionProof
    , foldProof
    , verifyInclusionProof
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
-- A complete inclusion proof from a leaf to the root.
--
-- Contains all the sibling information needed to recompute the root hash
-- from a leaf value.
data Proof a = Proof
    { proofSteps :: [ProofStep a]
    -- ^ Steps from leaf to root
    , proofRootJump :: Key
    -- ^ Jump path at the root node
    }
    deriving (Show, Eq)

-- |
-- Generate an inclusion proof for a key in the CSMT.
--
-- Traverses from root to the target key, collecting sibling hashes at each
-- branch. Returns 'Nothing' if the key is not in the tree.
buildInclusionProof
    :: (Monad m, GCompare d)
    => FromKV k v a
    -> Selector d Key (Indirect a)
    -> k
    -> Transaction m cf d ops (Maybe (Proof a))
buildInclusionProof FromKV{fromK} sel k = runMaybeT $ do
    let key = fromK k
    Indirect jump _ <- MaybeT $ query sel []
    guard $ isPrefixOf jump key
    rs <- go jump $ drop (length jump) key
    pure $ Proof{proofSteps = reverse rs, proofRootJump = jump}
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
-- Fold a proof to compute the expected root hash.
--
-- Starting from the leaf value, combines it with each sibling hash
-- using the tree's hashing functions to compute what the root hash
-- should be if the proof is valid.
--
-- The key is required to derive the direction and jump path at each step.
-- Since proof steps are ordered leaf-to-root but the key is root-to-leaf,
-- we consume key bits from the end first.
foldProof :: Hashing a -> Key -> a -> Proof a -> a
foldProof hashing key value Proof{proofSteps, proofRootJump} =
    rootHash hashing (Indirect proofRootJump rootValue)
  where
    keyAfterRoot = drop (length proofRootJump) key
    -- Reverse key so we can consume from the leaf end first
    rootValue = go value (reverse keyAfterRoot) proofSteps

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
                    error "foldProof: invalid proof step with zero consumed bits"

-- |
-- Verify an inclusion proof against the current tree root.
--
-- Computes the expected root hash from the proof and compares it to the
-- actual root hash in the tree. Returns 'True' if they match.
verifyInclusionProof
    :: (Eq a, Monad m, GCompare d)
    => FromKV k v a
    -> Selector d Key (Indirect a)
    -> Hashing a
    -> k
    -> v
    -> Proof a
    -> Transaction m cf d ops Bool
verifyInclusionProof FromKV{fromK, fromV} sel hashing k v proof = do
    let key = fromK k
        value = fromV v
    mv <- query sel []
    pure $ case mv of
        Just rootValue ->
            rootHash hashing rootValue == foldProof hashing key value proof
        Nothing -> False
