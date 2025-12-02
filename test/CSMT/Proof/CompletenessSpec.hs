{-# LANGUAGE OverloadedLists #-}

module CSMT.Proof.CompletenessSpec (spec)
where

import CSMT
    ( Backend (queryCSMT)
    , Direction (..)
    , combineHash
    )
import CSMT.Hashes (hashHashing)
import CSMT.Proof.Completeness
    ( collectValues
    , foldProof
    , generateProof
    )
import CSMT.Test.Lib
    ( evalPureFromEmptyDB
    , indirect
    , insertHashes
    , insertInts
    , intHash
    , intHashing
    , manyRandomPaths
    , pureBackendIdentity
    )
import Data.List (sort)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (forAll, property)

spec :: Spec
spec = do
    describe "collectValues" $ do
        it "collects all values for a simple tree of ints"
            $ property
            $ forAll manyRandomPaths
            $ \ks -> do
                let values = zipWith indirect ks [1 ..]
                    collected = evalPureFromEmptyDB $ do
                        insertInts values
                        collectValues pureBackendIdentity []
                collected
                    `shouldBe` sort values
        it "collects all values for a simple tree of hashes"
            $ property
            $ forAll manyRandomPaths
            $ \ks -> do
                let values = zipWith indirect ks (intHash <$> [1 ..])
                    collected = evalPureFromEmptyDB $ do
                        insertHashes values
                        collectValues pureBackendIdentity []
                collected
                    `shouldBe` sort values
    describe "generateProof" $ do
        it "can generate proof for empty tree"
            $ let mp = evalPureFromEmptyDB $ generateProof pureBackendIdentity []
              in  mp `shouldBe` Nothing
        it "can generate proof for simple tree"
            $ let
                mp = evalPureFromEmptyDB $ do
                    insertInts [indirect [L] 1]
                    generateProof pureBackendIdentity []
              in
                mp `shouldBe` Just []
        it "can generate proof for larger tree"
            $ let
                mp = evalPureFromEmptyDB $ do
                    insertInts
                        [ indirect [L] 1
                        , indirect [R] 2
                        ]
                    generateProof pureBackendIdentity []
              in
                mp `shouldBe` Just [(0, 1)]
        it "can generate proof for even larger tree"
            $ let
                mp = evalPureFromEmptyDB $ do
                    insertInts
                        [ indirect [L, L, L, L] 1
                        , indirect [L, R, L, L] 5
                        , indirect [L, R, L, R] 6
                        , indirect [R, R, R, R] 16
                        ]
                    generateProof pureBackendIdentity []
              in
                mp `shouldBe` Just [(1, 2), (0, 1), (0, 3)]
    describe "verifyProof" $ do
        it "can verify completeness proof for larger tree"
            $ let
                values =
                    [ indirect [L, L, L, L] 1
                    , indirect [L, R, L, L] 5
                    , indirect [L, R, L, R] 6
                    , indirect [R, R, R, R] 16
                    ]
                (mp, r) = evalPureFromEmptyDB $ do
                    insertInts values
                    mp' <- generateProof pureBackendIdentity []
                    r' <- queryCSMT pureBackendIdentity []
                    return (mp', r')
              in
                case mp of
                    Nothing -> error "expected a proof"
                    Just proof -> do
                        foldProof (combineHash intHashing) values proof
                            `shouldBe` r
        it "can verify completeness proof for random trees"
            $ property
            $ forAll manyRandomPaths
            $ \ks -> do
                let values = zipWith indirect ks [1 ..]
                    (mp, r) = evalPureFromEmptyDB $ do
                        insertInts values
                        mp' <- generateProof pureBackendIdentity []
                        r' <- queryCSMT pureBackendIdentity []
                        return (mp', r')
                case mp of
                    Nothing -> error "expected a proof"
                    Just proof -> do
                        foldProof (combineHash intHashing) (sort values) proof
                            `shouldBe` r
        it "can verify completeness proof for random trees of hashes"
            $ property
            $ forAll manyRandomPaths
            $ \ks -> do
                let values = zipWith indirect ks (intHash <$> [1 ..])
                    (mp, r) = evalPureFromEmptyDB $ do
                        insertHashes values
                        mp' <- generateProof pureBackendIdentity []
                        r' <- queryCSMT pureBackendIdentity []
                        return (mp', r')
                case mp of
                    Nothing -> error "expected a proof"
                    Just proof -> do
                        foldProof (combineHash hashHashing) (sort values) proof
                            `shouldBe` r
