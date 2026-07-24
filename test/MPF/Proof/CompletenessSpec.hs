{-# LANGUAGE OverloadedStrings #-}

module MPF.Proof.CompletenessSpec (spec) where

import Database.KV.Transaction (runTransactionUnguarded)
import MPF.Backend.Pure (mpfPureDatabase)
import MPF.Backend.Standalone (MPFStandalone (MPFStandaloneMPFCol))
import MPF.Hashes (MPFHash, mkMPFHash, mpfHashing)
import MPF.Insertion (MPFCompose)
import MPF.Interface
    ( HexDigit (..)
    , HexIndirect
    , HexKey
    , mkLeafIndirect
    )
import MPF.Proof.Completeness
    ( collectMPFLeaves
    , generateMPFCompletenessProof
    , verifyMPFCompletenessProof
    )
import MPF.Test.Lib
    ( getRootHashM
    , insertMPFM
    , mpfHashCodecs
    , runMPFPure'
    )
import Test.Hspec

-- | Build a small tree, then collect its completeness proof, its
-- complete leaf set, and its trusted root hash.
completenessProofFor
    :: [(HexKey, MPFHash)]
    -> ( Maybe (MPFCompose MPFHash)
       , [HexIndirect MPFHash]
       , Maybe MPFHash
       )
completenessProofFor inserts =
    fst $ runMPFPure' $ do
        mapM_ (uncurry insertMPFM) inserts
        (mProof, leaves) <-
            runTransactionUnguarded (mpfPureDatabase mpfHashCodecs)
                $ do
                    p <- generateMPFCompletenessProof MPFStandaloneMPFCol []
                    ls <- collectMPFLeaves MPFStandaloneMPFCol []
                    pure (p, ls)
        root <- getRootHashM
        pure (mProof, leaves, root)

testInserts :: [(HexKey, MPFHash)]
testInserts =
    [ ([HexDigit 1, HexDigit 1], mkMPFHash "a")
    , ([HexDigit 1, HexDigit 2], mkMPFHash "b")
    , ([HexDigit 2, HexDigit 3], mkMPFHash "c")
    ]

spec :: Spec
spec = describe "MPF.Proof.Completeness" $ do
    describe "verifyMPFCompletenessProof" $ do
        it "accepts an honest complete leaf set with the correct root"
            $ do
                let (mProof, leaves, trustedRoot) =
                        completenessProofFor testInserts
                case mProof of
                    Just proof ->
                        verifyMPFCompletenessProof
                            mpfHashing
                            trustedRoot
                            leaves
                            proof
                            `shouldBe` True
                    Nothing ->
                        expectationFailure "Expected a completeness proof"

        it "rejects a claimed leaf set with an extra leaf" $ do
            let (mProof, leaves, trustedRoot) =
                    completenessProofFor testInserts
                extra =
                    mkLeafIndirect
                        [HexDigit 9, HexDigit 9]
                        (mkMPFHash "x")
            case mProof of
                Just proof ->
                    verifyMPFCompletenessProof
                        mpfHashing
                        trustedRoot
                        (leaves ++ [extra])
                        proof
                        `shouldBe` False
                Nothing ->
                    expectationFailure "Expected a completeness proof"

        it "rejects a claimed leaf set with a leaf removed" $ do
            let (mProof, leaves, trustedRoot) =
                    completenessProofFor testInserts
            case mProof of
                Just proof
                    | length leaves >= 2 ->
                        verifyMPFCompletenessProof
                            mpfHashing
                            trustedRoot
                            (init leaves)
                            proof
                            `shouldBe` False
                    | otherwise ->
                        expectationFailure "Expected at least two leaves"
                Nothing ->
                    expectationFailure "Expected a completeness proof"

        it "rejects a correct leaf set against a wrong trusted root" $ do
            let (mProof, leaves, _) = completenessProofFor testInserts
                wrongRoot = Just (mkMPFHash "not-the-root")
            case mProof of
                Just proof ->
                    verifyMPFCompletenessProof
                        mpfHashing
                        wrongRoot
                        leaves
                        proof
                        `shouldBe` False
                Nothing ->
                    expectationFailure "Expected a completeness proof"
