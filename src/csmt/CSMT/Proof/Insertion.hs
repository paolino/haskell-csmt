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
    , mkInclusionProof
    , foldProof
    , verifyInclusionProof
    )
where

import CSMT.Interface
    ( Direction (..)
    , FromKV (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , addWithDirection
    , opposite
    )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Foldable (Foldable (..))
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
-- * The direction taken at this branch
-- * The jump path at this node
-- * The sibling's indirect value (needed to recompute parent hash)
data ProofStep a = ProofStep
    { stepDirection :: Direction
    -- ^ Direction taken at this branch
    , stepJump :: Key
    -- ^ Jump path at this node
    , stepSibiling :: Indirect a
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
mkInclusionProof
    :: (Monad m, GCompare d)
    => FromKV k v a
    -> Selector d Key (Indirect a)
    -> k
    -> Transaction m cf d ops (Maybe (Proof a))
mkInclusionProof FromKV{fromK} sel k = runMaybeT $ do
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
        stepSibiling <- MaybeT $ query sel (u <> [opposite x])
        let step =
                ProofStep
                    { stepDirection = x
                    , stepJump = jump
                    , stepSibiling
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
foldProof :: Hashing a -> a -> Proof a -> a
foldProof hashing value Proof{proofSteps, proofRootJump} =
    rootHash hashing (Indirect proofRootJump rootValue)
  where
    rootValue = foldl' step value proofSteps
    step acc ProofStep{stepDirection, stepSibiling, stepJump} =
        addWithDirection
            hashing
            stepDirection
            (Indirect stepJump acc)
            stepSibiling

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
    -> v
    -> Proof a
    -> Transaction m cf d ops Bool
verifyInclusionProof FromKV{fromV} sel hashing v proof = do
    let value = fromV v
    mv <- query sel []
    pure $ case mv of
        Just rootValue ->
            rootHash hashing rootValue == foldProof hashing value proof
        Nothing -> False
