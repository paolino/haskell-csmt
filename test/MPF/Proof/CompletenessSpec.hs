{-# LANGUAGE OverloadedStrings #-}

module MPF.Proof.CompletenessSpec (spec) where

import Data.ByteString qualified as B
import Data.List (isPrefixOf, nub)
import Data.Word (Word8)
import Database.KV.Transaction (query, runTransactionUnguarded)
import MPF.Backend.Pure (mpfPureDatabase)
import MPF.Backend.Standalone (MPFStandalone (MPFStandaloneMPFCol))
import MPF.Hashes (MPFHash, mkMPFHash, mpfHashing)
import MPF.Insertion (MPFCompose)
import MPF.Interface
    ( HexDigit (..)
    , HexIndirect (..)
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
import Test.QuickCheck
    ( Gen
    , Property
    , choose
    , counterexample
    , elements
    , expectFailure
    , forAll
    , property
    , vectorOf
    , (.&&.)
    , (===)
    )

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

-- QuickCheck generators (mirror MPF.Proof.ExclusionSpec's style).

genHexDigit :: Gen HexDigit
genHexDigit = HexDigit . fromIntegral <$> choose (0, 15 :: Int)

genFixedKey :: Int -> Gen HexKey
genFixedKey n = vectorOf n genHexDigit

genValueHash :: Gen MPFHash
genValueHash = mkMPFHash . B.pack <$> vectorOf 8 genWord8
  where
    genWord8 :: Gen Word8
    genWord8 = fromIntegral <$> choose (0, 255 :: Int)

genInserts :: [HexKey] -> Gen [(HexKey, MPFHash)]
genInserts keys = zip keys <$> vectorOf (length keys) genValueHash

-- | Generate a random tree: several distinct keys sharing a random
-- common base prefix (length 0-3) plus random suffixes, so the tree
-- exercises root jumps, compressed prefixes and multi-level branches.
genAnchoredInserts :: Gen [(HexKey, MPFHash)]
genAnchoredInserts = do
    n <- choose (2, 8)
    baseLen <- choose (0, 3)
    base <- genFixedKey baseLen
    suffixLen <- choose (2, 6)
    keys <-
        nub <$> vectorOf n ((base <>) <$> genFixedKey suffixLen)
    if length keys < 2
        then genAnchoredInserts
        else genInserts keys

-- | Collect every trie node as @(storage path, jump)@ by walking the
-- built tree. Drives the exact-node and within-jump prefix generators
-- below.
collectTrieNodes :: [(HexKey, MPFHash)] -> [(HexKey, HexKey)]
collectTrieNodes inserts =
    fst $ runMPFPure' $ do
        mapM_ (uncurry insertMPFM) inserts
        runTransactionUnguarded (mpfPureDatabase mpfHashCodecs)
            $ go []
  where
    go path = do
        mi <- query MPFStandaloneMPFCol path
        case mi of
            Nothing -> pure []
            Just HexIndirect{hexJump, hexIsLeaf}
                | hexIsLeaf -> pure [(path, hexJump)]
                | otherwise -> do
                    let base = path <> hexJump
                    rest <-
                        concat
                            <$> mapM
                                (\d -> go (base <> [d]))
                                [HexDigit n | n <- [0 .. 15]]
                    pure ((path, hexJump) : rest)

-- | A prefix that IS a real trie node: @query sel prefix@ returns
-- @Just@ (an actual node path, not a point inside a jump), excluding
-- the root so the prefix is a proper subtree. Genuinely varied —
-- nodes are picked uniformly across all depths (leaves and internal
-- branches), so multi-level anchors are exercised.
genExactNodePrefix :: [(HexKey, MPFHash)] -> Gen HexKey
genExactNodePrefix inserts =
    elements
        [ p
        | (p, _) <- collectTrieNodes inserts
        , not (null p)
        ]

-- | A prefix that falls INSIDE a node's jump: @query@ returns
-- @Nothing@ yet keys exist under it. This is the open #171 case.
-- Generates the tree together with such a prefix, retrying until the
-- tree has a node whose jump admits a proper non-empty sub-prefix.
genTreeAndWithinJumpPrefix :: Gen ([(HexKey, MPFHash)], HexKey)
genTreeAndWithinJumpPrefix = do
    inserts <- genAnchoredInserts
    let jumpNodes =
            [ (p, j)
            | (p, j) <- collectTrieNodes inserts
            , length j >= 2
            ]
    if null jumpNodes
        then genTreeAndWithinJumpPrefix
        else do
            (p, j) <- elements jumpNodes
            k <- choose (1, length j - 1)
            pure (inserts, p <> take k j)

-- | A tree together with an ABSENT prefix that runs past a terminal
-- leaf: pick an inserted key (a leaf) and extend it with extra
-- digits. The prefix is absent (every key is shorter than it, so none
-- has it as a prefix) yet extends past the leaf for that key — the
-- non-aligned absent shape of #171 that 'mkMPFExclusionProof' cannot
-- prove. Symmetric to 'genTreeAndWithinJumpPrefix'.
genTreeAndAbsentPastLeafPrefix :: Gen ([(HexKey, MPFHash)], HexKey)
genTreeAndAbsentPastLeafPrefix = do
    inserts <- genAnchoredInserts
    key <- elements (map fst inserts)
    extraLen <- choose (1, 4)
    extra <- genFixedKey extraLen
    pure (inserts, key <> extra)

-- | A prefix under which no inserted key lives (the absent case).
--
-- Scoped to prefixes that DIVERGE from every key: no key extends the
-- prefix (so nothing lives under it) and the prefix extends past no
-- key. The "extends past a leaf" shape is excluded because
-- 'mkMPFExclusionProof' cannot prove exclusion for a target that runs
-- deeper than a terminal leaf — that non-aligned-prefix case is the
-- open problem #171 (cf. the within-jump baseline below). This mirrors
-- ExclusionSpec, which keeps absent keys the same length as the tree
-- keys so an absent key can never extend past a key.
genAbsentPrefix :: [(HexKey, MPFHash)] -> Gen HexKey
genAbsentPrefix inserts = do
    plen <- choose (1, 6)
    pfx <- genFixedKey plen
    let keys = map fst inserts
    if any (isPrefixOf pfx) keys || any (`isPrefixOf` pfx) keys
        then genAbsentPrefix inserts
        else pure pfx

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

    describe "anchored completeness property tests" $ do
        it "honest exact-node prefix subtree verifies against the full root"
            $ property propAnchoredHonest

        it "tampering the anchor or leaves fails verification"
            $ property propAnchoredTamper

        it "absent prefix verifies via the exclusion path"
            $ property propAnchoredAbsent

        it "within-jump prefix completeness is an open problem (#171)"
            $ property propAnchoredWithinJumpOpen

        it "non-aligned absent prefix (past a leaf) is an open problem (#171)"
            $ property propAnchoredAbsentPastLeafOpen

proofShape :: Maybe (MPFCompletenessProof a) -> String
proofShape Nothing = "Nothing"
proofShape (Just MPFCompletenessWitness{}) = "Witness"
proofShape (Just MPFCompletenessEmpty{}) = "Empty"

propAnchoredHonest :: Property
propAnchoredHonest =
    forAll genAnchoredInserts $ \inserts ->
        forAll (genExactNodePrefix inserts) $ \pfx ->
            let (mProof, leaves, fullRoot) =
                    anchoredProofFor inserts pfx
            in  counterexample
                    ( "keys="
                        ++ show (map fst inserts)
                        ++ " prefix="
                        ++ show pfx
                        ++ " leaves="
                        ++ show leaves
                        ++ " proof="
                        ++ proofShape mProof
                        ++ " root="
                        ++ show fullRoot
                    )
                    $ case mProof of
                        Just proof ->
                            verifyMPFAnchoredCompletenessProof
                                mpfHashing
                                fullRoot
                                leaves
                                proof
                                === True
                        Nothing -> property False

propAnchoredTamper :: Property
propAnchoredTamper =
    forAll genAnchoredInserts $ \inserts ->
        forAll (genExactNodePrefix inserts) $ \pfx ->
            let (mProof, leaves, fullRoot) =
                    anchoredProofFor inserts pfx
                extra =
                    mkLeafIndirect
                        (pfx ++ [HexDigit 15, HexDigit 15, HexDigit 15])
                        (mkMPFHash "extra")
            in  counterexample
                    ( "keys="
                        ++ show (map fst inserts)
                        ++ " prefix="
                        ++ show pfx
                        ++ " proof="
                        ++ proofShape mProof
                    )
                    $ case mProof of
                        Just proof@MPFCompletenessWitness{} ->
                            verifyMPFAnchoredCompletenessProof
                                mpfHashing
                                fullRoot
                                leaves
                                (tamperAnchor proof)
                                === False
                                .&&. verifyMPFAnchoredCompletenessProof
                                    mpfHashing
                                    fullRoot
                                    (leaves ++ [extra])
                                    proof
                                === False
                                .&&. ( if null leaves
                                        then property True
                                        else
                                            verifyMPFAnchoredCompletenessProof
                                                mpfHashing
                                                fullRoot
                                                (init leaves)
                                                proof
                                                === False
                                     )
                        -- No witness (empty case) has an anchor to
                        -- tamper; the honest property covers presence.
                        _ -> property True

propAnchoredAbsent :: Property
propAnchoredAbsent =
    forAll genAnchoredInserts $ \inserts ->
        forAll (genAbsentPrefix inserts) $ \pfx ->
            let (mProof, leaves, fullRoot) =
                    anchoredProofFor inserts pfx
            in  counterexample
                    ( "keys="
                        ++ show (map fst inserts)
                        ++ " absentPrefix="
                        ++ show pfx
                        ++ " leaves="
                        ++ show leaves
                        ++ " proof="
                        ++ proofShape mProof
                    )
                    $ case mProof of
                        Just proof ->
                            verifyMPFAnchoredCompletenessProof
                                mpfHashing
                                fullRoot
                                leaves
                                proof
                                === True
                        Nothing -> property False

-- OPEN PROBLEM #171 (absent-side): a non-aligned absent prefix that runs past a
-- terminal leaf cannot be exclusion-proven. Documented failing baseline
-- (replay seed 1129545320). Do NOT "fix" here — see #171.
--
-- Symmetric to propAnchoredWithinJumpOpen: the prefix is genuinely absent (no
-- key under it) but extends past a leaf, so generation yields Nothing and the
-- honest claim fails. 'expectFailure' pins that known failure as a reproducible
-- baseline until #171 is solved, so the shape is documented, not silently
-- dropped from coverage.
propAnchoredAbsentPastLeafOpen :: Property
propAnchoredAbsentPastLeafOpen =
    expectFailure
        $ forAll genTreeAndAbsentPastLeafPrefix
        $ \(inserts, pfx) ->
            let (mProof, leaves, fullRoot) =
                    anchoredProofFor inserts pfx
            in  counterexample
                    ( "keys="
                        ++ show (map fst inserts)
                        ++ " absentPastLeafPrefix="
                        ++ show pfx
                        ++ " leaves="
                        ++ show leaves
                        ++ " proof="
                        ++ proofShape mProof
                    )
                    $ case mProof of
                        Just proof ->
                            verifyMPFAnchoredCompletenessProof
                                mpfHashing
                                fullRoot
                                leaves
                                proof
                                === True
                        Nothing -> property False

-- OPEN PROBLEM #171: within-jump prefix completeness is unsolved. Documented
-- failing baseline (replay seed 290458383). Do NOT "fix" here — see #171.
--
-- A prefix that falls inside a node jump (query returns Nothing but keys
-- exist under it) cannot currently be anchored to the full root: generation
-- yields Nothing and the honest claim fails. This property asserts the honest
-- claim, so it FAILS for every generated within-jump prefix; 'expectFailure'
-- pins that known failure as a reproducible baseline until #171 is solved.
propAnchoredWithinJumpOpen :: Property
propAnchoredWithinJumpOpen =
    expectFailure
        $ forAll genTreeAndWithinJumpPrefix
        $ \(inserts, pfx) ->
            let (mProof, leaves, fullRoot) =
                    anchoredProofFor inserts pfx
            in  counterexample
                    ( "keys="
                        ++ show (map fst inserts)
                        ++ " withinJumpPrefix="
                        ++ show pfx
                        ++ " leaves="
                        ++ show leaves
                        ++ " proof="
                        ++ proofShape mProof
                    )
                    $ case mProof of
                        Just proof ->
                            verifyMPFAnchoredCompletenessProof
                                mpfHashing
                                fullRoot
                                leaves
                                proof
                                === True
                        Nothing -> property False
