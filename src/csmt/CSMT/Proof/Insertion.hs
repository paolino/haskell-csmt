{-# LANGUAGE StrictData #-}

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
