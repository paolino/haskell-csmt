{-# LANGUAGE OverloadedLists #-}

module CSMT.Proof.InsertionSpec (spec)
where

import CSMT
    ( Direction (L, R)
    , Hashing
    , Key
    , Pure
    , emptyInMemoryDB
    , runPure
    )
import CSMT.Hashes (hashHashing)
import CSMT.Test.Lib
    ( delete
    , deleteMInt
    , genPaths
    , genSomePaths
    , identityFromKV
    , insertMHash
    , insertMInt
    , insertMList
    , inserted
    , intHash
    , intHashing
    , listHashing
    , verifyMHash
    , verifyMInt
    , verifyMList
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
    describe "proving" $ do
        it "verifies a simple fact"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMInt [L] 1
                    verifyMInt [L] 1
              in  r `shouldBe` True
        it "verifies a simple fact hash"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMHash [L] $ intHash (1 :: Int)
                    verifyMHash [L] $ intHash (1 :: Int)
              in  r `shouldBe` True
        it "verifies a simple fact"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMInt [R] 1
                    verifyMInt [R] 1
              in  r `shouldBe` True
        it "verifies a simple fact hash"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMHash [R] $ intHash (1 :: Int)
                    verifyMHash [R] $ intHash (1 :: Int)
              in  r `shouldBe` True
        it "verifies a deeper fact"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMInt [L, L] 10
                    verifyMInt [L, L] 10
              in  r `shouldBe` True
        it "verifies a deeper fact hash"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMHash [L, L] $ intHash (10 :: Int)
                    verifyMHash [L, L] $ intHash (10 :: Int)
              in  r `shouldBe` True
        it "verifies singleton [R,R] hash"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMHash [R, R] $ intHash (10 :: Int)
                    verifyMHash [R, R] $ intHash (10 :: Int)
              in  r `shouldBe` True
        it "verifies singleton [R,L] hash"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMHash [R, L] $ intHash (10 :: Int)
                    verifyMHash [R, L] $ intHash (10 :: Int)
              in  r `shouldBe` True
        it "verifies a deeper fact"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMInt [R, R] 10
                    insertMInt [R, L] 10
                    verifyMInt [R, R] 10
              in  r `shouldBe` True
        it "verifies a deeper fact list"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMList [R, R] [10 :: Int]
                    insertMList [R, L] [10 :: Int]
                    verifyMList [R, R] [10 :: Int]
              in  r `shouldBe` True
        it "verifies a deeper fact hash"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMHash [R, R] $ intHash 1
                    insertMHash [R, L] $ intHash 2
                    verifyMHash [R, L] $ intHash 2
              in  r `shouldBe` True
        it "verifies a deeper fact hash"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMHash [R, R] $ intHash (10 :: Int)
                    insertMHash [R, L] $ intHash (10 :: Int)
                    verifyMHash [R, R] $ intHash (10 :: Int)
              in  r `shouldBe` True
        it "verifies a deeper fact"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMInt [L, R, L] 42
                    verifyMInt [L, R, L] 42
              in  r `shouldBe` True
        it "verifies a deeper fact hash"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMHash [L, R, L] $ intHash (42 :: Int)
                    verifyMHash [L, R, L] $ intHash (42 :: Int)
              in  r `shouldBe` True
        it "verifies a factith siblings"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMInt [L] 10
                    insertMInt [R] 20
                    verifyMInt [L] 10
              in  r `shouldBe` True
        it "verifies another factith siblings"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMInt [L, L] 5
                    insertMInt [L, R] 15
                    insertMInt [R, L] 25
                    insertMInt [R, R] 35
                    verifyMInt [R, L] 25
              in  r `shouldBe` True
        it "verifies a fact against a full tree" $ do
            let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMInt [R, R] 10
                    insertMInt [R, L] 10
                    insertMInt [L, L] 11
                    verifyMInt [R, R] 10
            r `shouldBe` True
        let testRandomFactsInAFullTree
                :: (Int -> a)
                -> Hashing a
                -> (Key -> a -> Pure Key a a Bool)
                -> Property
            testRandomFactsInAFullTree h hashing vpf = forAll (elements [1 .. 8])
                $ \n -> forAll (listOf $ elements [0]) $ \ms ->
                    forAll (genPaths n) $ \keys -> forM_ ms $ \m -> do
                        let kvs = zip keys $ h . (1000 +) <$> [1 .. 2 ^ n]
                            (testKey, testValue) = kvs !! m
                            db = inserted identityFromKV hashing kvs
                            (r, _m) =
                                runPure db
                                    $ vpf testKey testValue
                        r `shouldBe` True
        it "verifies random facts in a full tree of ints"
            $ testRandomFactsInAFullTree @Int id intHashing verifyMInt
        it "verifies random facts in a full tree of lists"
            $ testRandomFactsInAFullTree @[Int] pure listHashing verifyMList
        it "verifies random facts in a full tree of hashes"
            $ testRandomFactsInAFullTree intHash hashHashing verifyMHash
        let testRandomFactsInASparseTree
                :: (Int -> a)
                -> Hashing a
                -> (Key -> a -> Pure Key a a Bool)
                -> Property
            testRandomFactsInASparseTree h hashing vpf = forAll (elements [1 .. 256])
                $ \n ->
                    forAll (genSomePaths n) $ \keys ->
                        forAll (listOf $ elements [0 .. length keys - 1]) $ \ks -> do
                            let kvs = zip keys $ h <$> [1 ..]
                                tree = inserted identityFromKV hashing kvs
                            forM_ ks $ \m -> do
                                let (testKey, testValue) = kvs !! m
                                    (r, _m) =
                                        runPure tree $ vpf testKey testValue
                                r `shouldBe` True
        it "verifies random facts in a sparse tree of ints"
            $ testRandomFactsInASparseTree @Int id intHashing verifyMInt
        it "verifies random facts in a sparse tree of lists"
            $ testRandomFactsInASparseTree @[Int] pure listHashing verifyMList
        it "verifies random facts in a sparse tree of hashes"
            $ testRandomFactsInASparseTree intHash hashHashing verifyMHash
        it "rejects the root deleted fact" $ do
            let (r, _) = runPure emptyInMemoryDB $ do
                    insertMInt [] 0
                    deleteMInt []
                    verifyMInt [] 0
            r `shouldBe` False
        it "rejects a non root deleted fact" $ do
            let (r, _) = runPure emptyInMemoryDB $ do
                    insertMInt [L] 0
                    deleteMInt [L]
                    verifyMInt [L] 0
            r `shouldBe` False
        let testRandomDeletedFactsInASparseTree
                :: (Int -> a)
                -> Hashing a
                -> (Key -> a -> Pure Key a a Bool)
                -> Property
            testRandomDeletedFactsInASparseTree h hashing vpf =
                forAll (elements [128 .. 256])
                    $ \n ->
                        forAll (genSomePaths n) $ \keys ->
                            forAll (listOf $ elements [0 .. length keys - 1])
                                $ \ks -> do
                                    let kvs = zip keys $ h <$> [1 ..]
                                        tree = inserted identityFromKV hashing kvs
                                    forM_ ks $ \m -> do
                                        let (testKey, testValue) = kvs !! m
                                            (r, _m) =
                                                runPure (delete identityFromKV hashing tree testKey)
                                                    $ vpf testKey testValue
                                        r `shouldBe` False
        it "rejects a random deleted int fact"
            $ testRandomDeletedFactsInASparseTree @Int id intHashing verifyMInt
        it "rejects a random deleted hash fact"
            $ testRandomDeletedFactsInASparseTree
                intHash
                hashHashing
                verifyMHash
