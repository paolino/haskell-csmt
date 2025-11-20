{-# LANGUAGE StrictData #-}

module CSMT.Proofs
    ( Proof (..)
    , ProofStep (..)
    , mkInclusionProof
    , foldProof
    , verifyInclusionProof
    )
where

import CSMT.Interface
    ( CSMT (CSMT, queryCSMT)
    , Direction (..)
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

data ProofStep a = ProofStep
    { stepDirection :: Direction
    , stepJump :: Key
    , stepSibiling :: Indirect a
    }
    deriving (Show, Eq)

data Proof a = Proof
    { proofSteps :: [ProofStep a]
    , proofRootJump :: Key
    }
    deriving (Show, Eq)

-- | Collect a proof for the presence of a key in the CSMT
mkInclusionProof
    :: Monad m
    => CSMT m k v a
    -> Key
    -> m (Maybe (Proof a))
mkInclusionProof CSMT{queryCSMT} key = runMaybeT $ do
    Indirect jump _ <- MaybeT $ queryCSMT []
    guard $ isPrefixOf jump key
    rs <- go jump $ drop (length jump) key
    pure $ Proof{proofSteps = reverse rs, proofRootJump = jump}
  where
    go _ [] = pure []
    go u (k : ks) = do
        Indirect jump _ <- MaybeT $ queryCSMT (u <> [k])
        guard $ isPrefixOf jump ks
        stepSibiling <- MaybeT $ queryCSMT (u <> [opposite k])
        let step =
                ProofStep
                    { stepDirection = k
                    , stepJump = jump
                    , stepSibiling
                    }
        (step :)
            <$> go
                (u <> (k : jump))
                (drop (length jump) ks)

-- | Fold a proof into a single value
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

-- | Verify a proof of given the included value
verifyInclusionProof
    :: (Eq a, Monad m)
    => CSMT m k v a
    -> Hashing a
    -> a
    -> Proof a
    -> m Bool
verifyInclusionProof csmt hashing value proof = do
    mv <- queryCSMT csmt []
    pure $ case mv of
        Just rootValue ->
            rootHash hashing rootValue == foldProof hashing value proof
        Nothing -> False
