{-# LANGUAGE StrictData #-}

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

-- | The input for a program to compute the root of a CSMT given all the values
type CompletenessProof = [(Int, Int)]

-- A function to compose two facts, some kind of project + concatenate + hash
type Compose a = Indirect a -> Indirect a -> a

-- | This should be implemented as part of verifying a completeness proof on the client side
-- which probably will not use this lbrary directly.
foldProof
    :: Compose a
    -- ^ function to compose two Indirect values
    -> [Indirect a]
    -- ^ list of indirect values that make up the CSMT under the internal root
    -> CompletenessProof
    -- ^ proof steps to validate completeness, Nothing signal length mismatches
    -> Maybe (Indirect a)
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

data TreeWithDifferentLengthsError = TreeWithDifferentLengthsError
    deriving (Show)

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
