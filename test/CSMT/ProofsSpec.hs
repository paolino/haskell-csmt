{-# LANGUAGE OverloadedLists #-}

module CSMT.ProofsSpec (spec)
where

import CSMT
    ( Direction (L, R)
    , Key
    , Pure
    , runPure
    )
import CSMT.Hashes (addHash)
import CSMT.Test.Lib
    ( deleteMInt
    , genPaths
    , genSomePaths
    , insertMInt
    , inserted
    , intHash
    , verifyMHash
    , verifyMInt
    )
import Data.Foldable (forM_)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck
    ( Property
    , forAll
    , listOf
    )
import Test.QuickCheck.Gen (elements)

spec :: Spec
spec = do
    describe "proving inclusion" $ do
        it "verifies a simple fact"
            $ let (r, _m) = runPure [] $ do
                    insertMInt [L] (1 :: Int)
                    verifyMInt [L] 1
              in  r `shouldBe` True
        it "verifies a deeper fact"
            $ let (r, _m) = runPure [] $ do
                    insertMInt [L, R, L] (42 :: Int)
                    verifyMInt [L, R, L] 42
              in  r `shouldBe` True
        it "verifies a fact with siblings"
            $ let (r, _m) = runPure [] $ do
                    insertMInt [L] (10 :: Int)
                    insertMInt [R] (20 :: Int)
                    verifyMInt [L] 10
              in  r `shouldBe` True
        it "verifies another fact with siblings"
            $ let (r, _m) = runPure [] $ do
                    insertMInt [L, L] (5 :: Int)
                    insertMInt [L, R] (15 :: Int)
                    insertMInt [R, L] (25 :: Int)
                    insertMInt [R, R] (35 :: Int)
                    verifyMInt [R, L] 25
              in  r `shouldBe` True
        let testRandomFactsInAFullTree
                :: (Int -> a) -> (a -> a -> a) -> (Key -> a -> Pure a Bool) -> Property
            testRandomFactsInAFullTree h a vpf = forAll (elements [1 .. 14])
                $ \n -> forAll (listOf $ elements [0 .. 2 ^ n - 1]) $ \ms ->
                    forAll (genPaths n) $ \keys -> forM_ ms $ \m -> do
                        let kvs = zip keys $ h <$> [1 .. 2 ^ n]
                            (testKey, testValue) = kvs !! m
                            (r, _m) = runPure (inserted a kvs) $ vpf testKey testValue
                        r `shouldBe` True
        it "verifies random facts in a full tree"
            $ testRandomFactsInAFullTree id (+) verifyMInt
        it "verifies random hash facts in a full tree"
            $ testRandomFactsInAFullTree intHash addHash verifyMHash
        let testRandomFactsInASparseTree
                :: (Int -> a) -> (a -> a -> a) -> (Key -> a -> Pure a Bool) -> Property
            testRandomFactsInASparseTree h a vpf = forAll (elements [128 .. 256])
                $ \n ->
                    forAll (genSomePaths n) $ \keys ->
                        forAll (listOf $ elements [0 .. length keys - 1]) $ \ks ->
                            forM_ ks $ \m -> do
                                let kvs = zip keys $ h <$> [1 ..]
                                    (testKey, testValue) = kvs !! m
                                    (r, _m) =
                                        runPure (inserted a kvs)
                                            $ vpf testKey testValue
                                r `shouldBe` True
        it "verifies random facts in a sparse tree"
            $ testRandomFactsInASparseTree id (+) verifyMInt
        it "verifies random hash facts in a sparse tree"
            $ testRandomFactsInASparseTree intHash addHash verifyMHash
        it "rejects the root deleted fact" $ do
            let (r, _) = runPure [] $ do
                    insertMInt [] 0
                    deleteMInt []
                    verifyMInt [] 0
            r `shouldBe` False
        it "rejects a non root deleted fact" $ do
            let (r, _) = runPure [] $ do
                    insertMInt [L] 0
                    deleteMInt [L]
                    verifyMInt [L] 0
            r `shouldBe` False
