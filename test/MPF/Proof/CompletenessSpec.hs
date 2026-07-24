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
    ( MPFCompletenessProof (..)
    , collectMPFLeaves
    , generateMPFAnchoredCompletenessProof
    , generateMPFCompletenessProof
    , verifyMPFAnchoredCompletenessProof
    , verifyMPFCompletenessProof
    )
import MPF.Proof.Insertion (MPFProofStep (..))
import MPF.Test.Lib
    ( fromHexKVIdentity
    , getRootHashM
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

-- | A non-@[]@ prefix that is a real internal node: keys
-- @[1,1]@ and @[1,2]@ live under it, while @[2,3]@ does not, so
-- the prefix subtree must be anchored to the full root.
anchoredPrefix :: HexKey
anchoredPrefix = [HexDigit 1]

-- | A prefix with no keys under it (emptiness case).
emptyPrefix :: HexKey
emptyPrefix = [HexDigit 9]

-- | Build a small tree, then generate an anchored completeness
-- proof at a prefix, collect the leaves under that prefix, and
-- read the full trusted root hash.
anchoredProofFor
    :: [(HexKey, MPFHash)]
    -> HexKey
    -> ( Maybe (MPFCompletenessProof MPFHash)
       , [HexIndirect MPFHash]
       , Maybe MPFHash
       )
anchoredProofFor inserts prefix =
    fst $ runMPFPure' $ do
        mapM_ (uncurry insertMPFM) inserts
        (mProof, leaves) <-
            runTransactionUnguarded (mpfPureDatabase mpfHashCodecs)
                $ do
                    p <-
                        generateMPFAnchoredCompletenessProof
                            []
                            fromHexKVIdentity
                            mpfHashing
                            MPFStandaloneMPFCol
                            prefix
                    ls <- collectMPFLeaves MPFStandaloneMPFCol prefix
                    pure (p, ls)
        root <- getRootHashM
        pure (mProof, leaves, root)

-- | Flip a sibling digest in every anchor step so the anchor no
-- longer reconstructs the full root.
tamperAnchor
    :: MPFCompletenessProof MPFHash
    -> MPFCompletenessProof MPFHash
tamperAnchor (MPFCompletenessWitness subtree steps) =
    MPFCompletenessWitness subtree (map tamperStep steps)
tamperAnchor p = p

tamperStep :: MPFProofStep MPFHash -> MPFProofStep MPFHash
tamperStep s@ProofStepLeaf{} =
    s{pslNeighborValueDigest = mkMPFHash "tampered"}
tamperStep s@ProofStepFork{} =
    s{psfMerkleRoot = mkMPFHash "tampered"}
tamperStep s@ProofStepBranch{} =
    s
        { psbSiblingHashes =
            map (fmap (const (mkMPFHash "tampered")))
                (psbSiblingHashes s)
        }

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

    describe "verifyMPFAnchoredCompletenessProof" $ do
        it "accepts an honest prefix subtree against the full root" $ do
            let (mProof, leaves, fullRoot) =
                    anchoredProofFor testInserts anchoredPrefix
            case mProof of
                Just proof ->
                    verifyMPFAnchoredCompletenessProof
                        mpfHashing
                        fullRoot
                        leaves
                        proof
                        `shouldBe` True
                Nothing ->
                    expectationFailure "Expected an anchored proof"

        it "rejects a claimed leaf set with an extra leaf" $ do
            let (mProof, leaves, fullRoot) =
                    anchoredProofFor testInserts anchoredPrefix
                extra =
                    mkLeafIndirect
                        [HexDigit 1, HexDigit 9, HexDigit 9]
                        (mkMPFHash "x")
            case mProof of
                Just proof ->
                    verifyMPFAnchoredCompletenessProof
                        mpfHashing
                        fullRoot
                        (leaves ++ [extra])
                        proof
                        `shouldBe` False
                Nothing ->
                    expectationFailure "Expected an anchored proof"

        it "rejects a claimed leaf set with a leaf under the prefix removed"
            $ do
                let (mProof, leaves, fullRoot) =
                        anchoredProofFor testInserts anchoredPrefix
                case mProof of
                    Just proof
                        | length leaves >= 2 ->
                            verifyMPFAnchoredCompletenessProof
                                mpfHashing
                                fullRoot
                                (init leaves)
                                proof
                                `shouldBe` False
                        | otherwise ->
                            expectationFailure
                                "Expected at least two leaves under the prefix"
                    Nothing ->
                        expectationFailure "Expected an anchored proof"

        it "rejects a tampered anchor step" $ do
            let (mProof, leaves, fullRoot) =
                    anchoredProofFor testInserts anchoredPrefix
            case mProof of
                Just proof ->
                    verifyMPFAnchoredCompletenessProof
                        mpfHashing
                        fullRoot
                        leaves
                        (tamperAnchor proof)
                        `shouldBe` False
                Nothing ->
                    expectationFailure "Expected an anchored proof"

        it "rejects a correct subtree against a wrong trusted root" $ do
            let (mProof, leaves, _) =
                    anchoredProofFor testInserts anchoredPrefix
                wrongRoot = Just (mkMPFHash "not-the-root")
            case mProof of
                Just proof ->
                    verifyMPFAnchoredCompletenessProof
                        mpfHashing
                        wrongRoot
                        leaves
                        proof
                        `shouldBe` False
                Nothing ->
                    expectationFailure "Expected an anchored proof"

        it "accepts an empty prefix via the exclusion path" $ do
            let (mProof, leaves, fullRoot) =
                    anchoredProofFor testInserts emptyPrefix
            case mProof of
                Just proof ->
                    verifyMPFAnchoredCompletenessProof
                        mpfHashing
                        fullRoot
                        leaves
                        proof
                        `shouldBe` True
                Nothing ->
                    expectationFailure "Expected an anchored proof"
