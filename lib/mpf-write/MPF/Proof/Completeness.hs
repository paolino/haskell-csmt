{-# LANGUAGE StrictData #-}

-- |
-- Module      : MPF.Proof.Completeness
-- Description : Completeness proofs for MPFs
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module provides completeness proofs for Merkle Patricia
-- Forests. A completeness proof proves that a set of leaves
-- comprises ALL leaves in the tree.
--
-- The proof is an 'MPFCompose' tree structure that can be scanned
-- to recompute the root hash. The verifier checks that the leaves
-- in the proof match the provided leaves, and that the computed
-- root hash matches the trusted root.
module MPF.Proof.Completeness
    ( collectMPFLeaves
    , generateMPFCompletenessProof
    , generateMPFAnchoredCompletenessProof
    , foldMPFCompletenessProof
    , verifyMPFCompletenessProof
    , verifyMPFAnchoredCompletenessProof
    , MPFCompletenessProof (..)
    , extractLeaves
    )
where

import Data.List (sort)
import Data.Map.Strict qualified as Map
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , query
    )
import MPF.Hashes (MPFHashing (..))
import MPF.Insertion (MPFCompose (..), fetchChildTree, scanMPFCompose)
import MPF.Interface
    ( FromHexKV (..)
    , HexDigit (..)
    , HexIndirect (..)
    , HexKey
    , mkLeafIndirect
    , prefixHex
    )
import MPF.Proof.Exclusion
    ( MPFExclusionProof
    , mkMPFExclusionProof
    , verifyMPFExclusionProof
    )
import MPF.Proof.Insertion
    ( MPFProofStep
    , foldMPFProofFrom
    , mkMPFNodeInclusionSteps
    )

-- |
-- Collect all leaf values from the MPF trie.
-- The prefix scopes the query to a subtree.
--
-- Returns leaves with their full key path as 'hexJump' and
-- their value hash as 'hexValue'.
collectMPFLeaves
    :: (Monad m, GCompare d)
    => Selector d HexKey (HexIndirect a)
    -> HexKey
    -- ^ Prefix (use @[]@ for root)
    -> Transaction m cf d op [HexIndirect a]
collectMPFLeaves sel = navigate
  where
    navigate currentKey = do
        mi <- query sel currentKey
        case mi of
            Nothing -> pure []
            Just HexIndirect{hexJump, hexValue, hexIsLeaf}
                | hexIsLeaf ->
                    pure [mkLeafIndirect hexJump hexValue]
                | otherwise -> do
                    let base = currentKey <> hexJump
                    concat
                        <$> mapM
                            ( \d -> do
                                cs <- navigate (base <> [d])
                                pure
                                    $ map
                                        (prefixHex (hexJump ++ [d]))
                                        cs
                            )
                            allDigits
    allDigits = [HexDigit n | n <- [0 .. 15]]

-- |
-- Generate a completeness proof for the entire MPF trie.
-- The prefix scopes the query to a subtree.
--
-- Returns the trie as an 'MPFCompose' tree, or 'Nothing' if
-- the tree is empty.
generateMPFCompletenessProof
    :: (Monad m, GCompare d)
    => Selector d HexKey (HexIndirect a)
    -> HexKey
    -- ^ Prefix (use @[]@ for root)
    -> Transaction m cf d op (Maybe (MPFCompose a))
generateMPFCompletenessProof sel prefix = do
    mi <- query sel prefix
    case mi of
        Nothing -> pure Nothing
        Just HexIndirect{hexJump, hexValue, hexIsLeaf}
            | hexIsLeaf ->
                pure
                    $ Just
                    $ MPFComposeLeaf
                    $ mkLeafIndirect hexJump hexValue
            | otherwise -> do
                children <- fetchChildTree sel (prefix <> hexJump)
                pure $ Just $ MPFComposeBranch hexJump children

-- |
-- A completeness proof for the leaves under an internal-node
-- prefix, verified against the FULL published tree root.
--
-- * 'MPFCompletenessWitness' — the prefix has at least one leaf:
--   'mcpSubtree' is the subtree under the prefix and
--   'mcpAnchorSteps' are the inclusion steps from the subtree root
--   outward to the full tree root.
--
-- * 'MPFCompletenessEmpty' — the prefix has no leaves under the
--   trusted root; carried as an 'MPFExclusionProof' for the prefix.
data MPFCompletenessProof a
    = MPFCompletenessWitness
        { mcpSubtree :: MPFCompose a
        -- ^ Subtree under the prefix
        , mcpAnchorSteps :: [MPFProofStep a]
        -- ^ Subtree root -> full tree root inclusion steps
        }
    | MPFCompletenessEmpty (MPFExclusionProof a)
    -- ^ Prefix absent under the trusted root
    deriving (Show, Eq)

-- |
-- Generate an anchored completeness proof for the leaves under a
-- prefix, against the full tree root scoped by @scope@.
--
-- When the prefix is a populated internal node, builds the subtree
-- ('generateMPFCompletenessProof') plus the anchor inclusion steps
-- from the subtree root to the full root ('mkMPFNodeInclusionSteps').
-- When nothing exists under the prefix, returns the exclusion case.
generateMPFAnchoredCompletenessProof
    :: (Monad m, GCompare d)
    => HexKey
    -- ^ Scope prefix (use @[]@ for the full tree root)
    -> FromHexKV HexKey v a
    -> MPFHashing a
    -> Selector d HexKey (HexIndirect a)
    -> HexKey
    -- ^ Target prefix to prove complete
    -> Transaction m cf d op (Maybe (MPFCompletenessProof a))
generateMPFAnchoredCompletenessProof scope fhkv hashing sel target = do
    mNode <- query sel target
    case mNode of
        Nothing -> do
            mexcl <- mkMPFExclusionProof scope fhkv hashing sel target
            pure $ MPFCompletenessEmpty <$> mexcl
        Just _ -> do
            mSubtree <- generateMPFCompletenessProof sel target
            mAnchor <- mkMPFNodeInclusionSteps scope hashing sel target
            pure $ case (mSubtree, mAnchor) of
                (Just subtree, Just anchor) ->
                    Just $ MPFCompletenessWitness subtree anchor
                _ -> Nothing

-- |
-- Verify a completeness proof by computing the tree root hash.
--
-- Extracts leaves from the proof, checks they match the provided
-- leaves (sorted), then scans the proof tree to compute the root
-- hash. Returns 'Nothing' if the leaves do not match.
foldMPFCompletenessProof
    :: (Ord a)
    => MPFHashing a
    -> [HexIndirect a]
    -> MPFCompose a
    -> Maybe a
foldMPFCompletenessProof hashing leaves proof =
    let extracted = extractLeaves proof
        (rootIndirect, _) = scanMPFCompose [] hashing proof
        computedRoot = hexValue rootIndirect
    in  if sort extracted == sort leaves
            then Just computedRoot
            else Nothing

-- |
-- Verify a completeness proof against a trusted root.
--
-- Checks the claimed complete leaf set against the proof and
-- compares the recomputed root hash to the trusted root.
verifyMPFCompletenessProof
    :: (Ord a)
    => MPFHashing a
    -> Maybe a
    -- ^ Trusted root hash
    -> [HexIndirect a]
    -- ^ Claimed complete leaf set
    -> MPFCompose a
    -- ^ The completeness proof
    -> Bool
verifyMPFCompletenessProof hashing trustedRoot leaves proof =
    trustedRoot == foldMPFCompletenessProof hashing leaves proof

-- |
-- Verify an anchored completeness proof against the full trusted
-- root.
--
-- For a witness, checks the claimed leaf set is complete for the
-- prefix subtree, recomputes the subtree root, lifts it through the
-- anchor steps to the full root, and compares to the trusted root.
-- For the empty case, the claimed leaf set must be empty and the
-- embedded exclusion proof must verify against the trusted root.
verifyMPFAnchoredCompletenessProof
    :: (Ord a)
    => MPFHashing a
    -> Maybe a
    -- ^ Trusted full tree root
    -> [HexIndirect a]
    -- ^ Claimed complete leaf set under the prefix
    -> MPFCompletenessProof a
    -> Bool
verifyMPFAnchoredCompletenessProof hashing trustedRoot leaves proof =
    case proof of
        MPFCompletenessWitness{mcpSubtree, mcpAnchorSteps} ->
            case foldMPFCompletenessProof hashing leaves mcpSubtree of
                Nothing -> False
                Just subtreeRoot ->
                    Just
                        ( foldMPFProofFrom
                            hashing
                            subtreeRoot
                            mcpAnchorSteps
                        )
                        == trustedRoot
        MPFCompletenessEmpty exclusion ->
            null leaves
                && verifyMPFExclusionProof hashing trustedRoot exclusion

-- |
-- Extract all leaves from an 'MPFCompose' tree with their full
-- key paths reconstructed.
extractLeaves :: MPFCompose a -> [HexIndirect a]
extractLeaves = go []
  where
    go pfx (MPFComposeLeaf HexIndirect{hexJump, hexValue}) =
        [mkLeafIndirect (pfx ++ hexJump) hexValue]
    go pfx (MPFComposeBranch jmp children) =
        concatMap
            (\(d, c) -> go (pfx ++ jmp ++ [d]) c)
            (Map.toList children)
