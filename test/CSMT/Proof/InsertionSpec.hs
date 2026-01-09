{-# LANGUAGE OverloadedLists #-}

module CSMT.Proof.InsertionSpec (spec)
where

import CSMT
    ( Direction (L, R)
    , Hashing
    , Key
    , StandaloneCodecs
    )
import CSMT.Backend.Pure
    ( Pure
    , emptyInMemoryDB
    , runPure
    )
import CSMT.Hashes (hashHashing)
import CSMT.Test.Lib
    ( delete
    , deleteMWord64
    , genPaths
    , genSomePaths
    , hashCodecs
    , identityFromKV
    , insertMHash
    , insertMList
    , insertMWord64
    , inserted
    , intHash
    , listHashing
    , listOfWord64Codecs
    , verifyMHash
    , verifyMList
    , verifyMWord64
    , word64Codecs
    , word64Hashing
    )
import Data.Foldable (forM_)
import Data.Word (Word64)
import Test.Hspec (Spec, describe, it, shouldBe, xit)
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
                    insertMWord64 [L] 1
                    verifyMWord64 [L] 1
              in  r `shouldBe` True
        it "verifies a simple fact hash"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMHash [L] $ intHash (1 :: Word64)
                    verifyMHash [L] $ intHash (1 :: Word64)
              in  r `shouldBe` True
        it "verifies a simple fact"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMWord64 [R] 1
                    verifyMWord64 [R] 1
              in  r `shouldBe` True
        it "verifies a simple fact hash"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMHash [R] $ intHash (1 :: Word64)
                    verifyMHash [R] $ intHash (1 :: Word64)
              in  r `shouldBe` True
        it "verifies a deeper fact"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMWord64 [L, L] 10
                    verifyMWord64 [L, L] 10
              in  r `shouldBe` True
        it "verifies a deeper fact hash"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMHash [L, L] $ intHash (10 :: Word64)
                    verifyMHash [L, L] $ intHash (10 :: Word64)
              in  r `shouldBe` True
        it "verifies singleton [R,R] hash"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMHash [R, R] $ intHash (10 :: Word64)
                    verifyMHash [R, R] $ intHash (10 :: Word64)
              in  r `shouldBe` True
        it "verifies singleton [R,L] hash"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMHash [R, L] $ intHash (10 :: Word64)
                    verifyMHash [R, L] $ intHash (10 :: Word64)
              in  r `shouldBe` True
        it "verifies a deeper fact"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMWord64 [R, R] 10
                    insertMWord64 [R, L] 10
                    verifyMWord64 [R, R] 10
              in  r `shouldBe` True
        it "verifies a deeper fact list"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMList [R, R] [10 :: Word64]
                    insertMList [R, L] [10 :: Word64]
                    verifyMList [R, R] [10 :: Word64]
              in  r `shouldBe` True
        it "verifies a deeper fact hash"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMHash [R, R] $ intHash 1
                    insertMHash [R, L] $ intHash 2
                    verifyMHash [R, L] $ intHash 2
              in  r `shouldBe` True
        it "verifies a deeper fact hash"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMHash [R, R] $ intHash (10 :: Word64)
                    insertMHash [R, L] $ intHash (10 :: Word64)
                    verifyMHash [R, R] $ intHash (10 :: Word64)
              in  r `shouldBe` True
        it "verifies a deeper fact"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMWord64 [L, R, L] 42
                    verifyMWord64 [L, R, L] 42
              in  r `shouldBe` True
        it "verifies a deeper fact hash"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMHash [L, R, L] $ intHash (42 :: Word64)
                    verifyMHash [L, R, L] $ intHash (42 :: Word64)
              in  r `shouldBe` True
        it "verifies a factith siblings"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMWord64 [L] 10
                    insertMWord64 [R] 20
                    verifyMWord64 [L] 10
              in  r `shouldBe` True
        it "verifies another factith siblings"
            $ let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMWord64 [L, L] 5
                    insertMWord64 [L, R] 15
                    insertMWord64 [R, L] 25
                    insertMWord64 [R, R] 35
                    verifyMWord64 [R, L] 25
              in  r `shouldBe` True
        it "verifies a fact against a full tree" $ do
            let (r, _m) = runPure emptyInMemoryDB $ do
                    insertMWord64 [R, R] 10
                    insertMWord64 [R, L] 10
                    insertMWord64 [L, L] 11
                    verifyMWord64 [R, R] 10
            r `shouldBe` True
        let testRandomFactsInAFullTree
                :: (Word64 -> a)
                -> StandaloneCodecs Key a a
                -> Hashing a
                -> (Key -> a -> Pure Bool)
                -> Property
            testRandomFactsInAFullTree h codecs hashing vpf = forAll (elements [1 .. 8])
                $ \n -> forAll (listOf $ elements [0]) $ \ms ->
                    forAll (genPaths n) $ \keys -> forM_ ms $ \m -> do
                        let kvs = zip keys $ h . (1000 +) <$> [1 .. 2 ^ n]
                            (testKey, testValue) = kvs !! m
                            db = inserted codecs identityFromKV hashing kvs
                            (r, _m) =
                                runPure db
                                    $ vpf testKey testValue
                        r `shouldBe` True
        it "verifies random facts in a full tree of ints"
            $ testRandomFactsInAFullTree
                id
                word64Codecs
                word64Hashing
                verifyMWord64
        it "verifies random facts in a full tree of lists"
            $ testRandomFactsInAFullTree
                pure
                listOfWord64Codecs
                listHashing
                verifyMList
        it "verifies random facts in a full tree of hashes"
            $ testRandomFactsInAFullTree intHash hashCodecs hashHashing verifyMHash
        let testRandomFactsInASparseTree
                :: (Word64 -> a)
                -> StandaloneCodecs Key a a
                -> Hashing a
                -> (Key -> a -> Pure Bool)
                -> Property
            testRandomFactsInASparseTree h codecs hashing vpf = forAll (elements [1 .. 256])
                $ \n ->
                    forAll (genSomePaths n) $ \keys ->
                        forAll (listOf $ elements [0 .. length keys - 1]) $ \ks -> do
                            let kvs = zip keys $ h <$> [1 ..]
                                tree = inserted codecs identityFromKV hashing kvs
                            forM_ ks $ \m -> do
                                let (testKey, testValue) = kvs !! m
                                    (r, _m) =
                                        runPure tree $ vpf testKey testValue
                                r `shouldBe` True
        it "verifies random facts in a sparse tree of ints"
            $ testRandomFactsInASparseTree
                id
                word64Codecs
                word64Hashing
                verifyMWord64
        xit "verifies random facts in a sparse tree of lists"
            $ testRandomFactsInASparseTree
                pure
                listOfWord64Codecs
                listHashing
                verifyMList
        it "verifies random facts in a sparse tree of hashes"
            $ testRandomFactsInASparseTree
                intHash
                hashCodecs
                hashHashing
                verifyMHash
        it "rejects the root deleted fact" $ do
            let (r, _) = runPure emptyInMemoryDB $ do
                    insertMWord64 [] 0
                    deleteMWord64 []
                    verifyMWord64 [] 0
            r `shouldBe` False
        it "rejects a non root deleted fact" $ do
            let (r, _) = runPure emptyInMemoryDB $ do
                    insertMWord64 [L] 0
                    deleteMWord64 [L]
                    verifyMWord64 [L] 0
            r `shouldBe` False
        let testRandomDeletedFactsInASparseTree
                :: (Word64 -> a)
                -> StandaloneCodecs Key a a
                -> Hashing a
                -> (Key -> a -> Pure Bool)
                -> Property
            testRandomDeletedFactsInASparseTree h codecs hashing vpf =
                forAll (elements [128 .. 256])
                    $ \n ->
                        forAll (genSomePaths n) $ \keys ->
                            forAll (listOf $ elements [0 .. length keys - 1])
                                $ \ks -> do
                                    let kvs = zip keys $ h <$> [1 ..]
                                        tree = inserted codecs identityFromKV hashing kvs
                                    forM_ ks $ \m -> do
                                        let (testKey, testValue) = kvs !! m
                                            (r, _m) =
                                                runPure (delete codecs identityFromKV hashing tree testKey)
                                                    $ vpf testKey testValue
                                        r `shouldBe` False
        it "rejects a random deleted int fact"
            $ testRandomDeletedFactsInASparseTree @Word64
                id
                word64Codecs
                word64Hashing
                verifyMWord64
        it "rejects a random deleted hash fact"
            $ testRandomDeletedFactsInASparseTree
                intHash
                hashCodecs
                hashHashing
                verifyMHash
