{-# LANGUAGE StrictData #-}

-- |
-- Module      : CSMT.Proof.Completeness
-- Description : Completeness proofs for CSMTs
-- Copyright   : (c) Paolo Veronelli, 2024
-- License     : Apache-2.0
--
-- This module provides functionality for completeness proofs - proving
-- that a set of values comprises the entire tree contents.
--
-- A completeness proof is a sequence of merge operations that, when
-- applied to a list of leaf values, produces the root hash. This allows
-- clients to verify they have received all values in the tree.
module CSMT.Proof.Completeness
    ( CompletenessProof
    , foldProof
    , collectValues
    , generateProof
    )
where

import CSMT.Interface
    ( Direction (..)
    , Indirect (..)
    , Key
    , compareKeys
    , prefix
    )
import Data.Map.Strict qualified as Map
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , query
    )

-- |
-- A completeness proof as a sequence of merge operations.
--
-- Each pair (i, j) indicates that values at indices i and j should be
-- combined to produce a parent hash. The final result should match the root.
type CompletenessProof = [(Int, Int)]

-- | A function to compose two indirect values into a combined hash.
type Compose a = Indirect a -> Indirect a -> a

-- |
-- Verify a completeness proof by folding the merge operations.
--
-- Takes a list of leaf values and applies the proof's merge operations
-- to compute the root. Returns 'Nothing' if the proof is invalid.
--
-- This function is intended for client-side verification where the client
-- receives the list of all values and the proof.
foldProof
    :: Compose a
    -- ^ Function to compose two Indirect values
    -> [Indirect a]
    -- ^ List of indirect values (leaves of the CSMT)
    -> CompletenessProof
    -- ^ Proof steps (merge operations to apply)
    -> Maybe (Indirect a)
    -- ^ Computed root, or Nothing if proof is invalid
foldProof compose values = go (Map.fromList $ zip [0 ..] values)
  where
    go m [] = Just $ m Map.! 0
    go m ((i, j) : xs) =
        let (Indirect pri vi) = m Map.! i
            (Indirect prj vj) = m Map.! j
        in  case compareKeys pri prj of
                (common, _ : si, _ : sj) ->
                    let
                        v =
                            Indirect
                                { jump = common
                                , value =
                                    compose
                                        Indirect{jump = si, value = vi}
                                        Indirect{jump = sj, value = vj}
                                }
                        m' = Map.insert i v m
                    in
                        go m' xs
                _ -> Nothing

-- | Error type for malformed trees (unused, kept for documentation).
data TreeWithDifferentLengthsError = TreeWithDifferentLengthsError
    deriving (Show)

-- |
-- Collect all leaf values from a subtree.
--
-- Traverses the tree depth-first, collecting all leaf indirect values
-- with their full paths from the given starting key.
collectValues
    :: (Monad m, GCompare d)
    => Selector d Key (Indirect a)
    -> Key
    -> Transaction m cf d op [Indirect a]
collectValues sel = go
  where
    go key = do
        mi <- query sel key
        case mi of
            Nothing -> pure []
            Just indirect@(Indirect jump _) -> do
                l <- go (key <> jump <> [L])
                r <- go (key <> jump <> [R])
                if null l && null r
                    then pure [indirect]
                    else
                        pure
                            $ prefix jump
                                <$> ( (prefix [L] <$> l)
                                        <> (prefix [R] <$> r)
                                    )

-- |
-- Generate a completeness proof for a subtree.
--
-- Traverses the tree and generates a sequence of merge operations that,
-- when applied to the collected leaf values, will produce the root hash.
generateProof
    :: forall m d a cf op
     . (Monad m, GCompare d)
    => Selector d Key (Indirect a)
    -> Key
    -> Transaction m cf d op (Maybe CompletenessProof)
generateProof sel = fmap (fmap fst) . go 0
  where
    go
        :: Int
        -> Key
        -> Transaction m cf d op (Maybe (CompletenessProof, (Int, Int)))
    go n key = do
        mi <- query sel key
        case mi of
            Nothing -> pure Nothing
            Just (Indirect jump _) -> do
                let leftKey = key <> jump <> [L]
                    rightKey = key <> jump <> [R]
                ml <- go n leftKey
                case ml of
                    Nothing -> pure $ Just ([], (n + 1, n))
                    Just (lxs, (n', li)) -> do
                        mr <- go n' rightKey
                        case mr of
                            Nothing -> error "Right subtree missing"
                            Just (rxs, (n'', ri)) ->
                                pure
                                    $ Just
                                        ( lxs
                                            ++ rxs
                                            ++ [(li, ri)]
                                        , (n'', n)
                                        )
