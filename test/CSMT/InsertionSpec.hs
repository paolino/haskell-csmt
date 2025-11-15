{-# LANGUAGE OverloadedLists #-}

module CSMT.InsertionSpec (spec)
where

import CSMT
    ( Direction (L, R)
    )
import CSMT.Test.Lib
    ( genPaths
    , indirect
    , insertInt
    , inserted
    , summed
    )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck
    ( forAll
    )
import Test.QuickCheck.Gen (elements)

spec :: Spec
spec = do
    describe "inserting" $ do
        it "inserts 1 key L"
            $ let
                rs = insertInt [] [L] (1 :: Int)
              in
                rs `shouldBe` [([], indirect [L] 1)]
        it "inserts 1 key R"
            $ let
                rs = insertInt [] [R] (1 :: Int)
              in
                rs `shouldBe` [([], indirect [R] 1)]
        it "inserts 1 key LL"
            $ let
                rs = insertInt [] [L, L] (1 :: Int)
              in
                rs `shouldBe` [([], indirect [L, L] 1)]
        it "inserts 1 key LR"
            $ let
                rs = insertInt [] [R, R] (1 :: Int)
              in
                rs `shouldBe` [([], indirect [R, R] 1)]
        it "inserts 2 keys R and L"
            $ let
                rs0 = insertInt [] [L] (1 :: Int)
                rs1 = insertInt rs0 [R] (2 :: Int)
              in
                rs1
                    `shouldBe` [ ([], indirect [] 3)
                               , ([L], indirect [] 1)
                               , ([R], indirect [] 2)
                               ]
        it "inserts 2 keys L and R"
            $ let
                rs0 = insertInt [] [R] (2 :: Int)
                rs1 = insertInt rs0 [L] (1 :: Int)
              in
                rs1
                    `shouldBe` [ ([], indirect [] 3)
                               , ([L], indirect [] 1)
                               , ([R], indirect [] 2)
                               ]
        it "inserts 2 keys LL and LR"
            $ let
                rs0 = insertInt [] [L, L] (1 :: Int)
                rs1 = insertInt rs0 [L, R] (2 :: Int)
              in
                rs1
                    `shouldBe` [ ([], indirect [L] 3)
                               , ([L, L], indirect [] 1)
                               , ([L, R], indirect [] 2)
                               ]

        it "inserts 2 keys RR and LL"
            $ let
                rs0 = insertInt [] [L, L] (1 :: Int)
                rs1 = insertInt rs0 [R, R] (2 :: Int)
              in
                rs1
                    `shouldBe` [ ([], indirect [] 3)
                               , ([L], indirect [L] 1)
                               , ([R], indirect [R] 2)
                               ]
        it "inserts 2 keys LR and RL"
            $ let
                rs0 = insertInt [] [L, R] (1 :: Int)
                rs1 = insertInt rs0 [R, L] (2 :: Int)
              in
                rs1
                    `shouldBe` [ ([], indirect [] 3)
                               , ([L], indirect [R] 1)
                               , ([R], indirect [L] 2)
                               ]
        it "inserts 3 keys LL, RL, LR"
            $ let
                rs0 = insertInt [] [L, L] (1 :: Int)
                rs1 = seq rs0 $ insertInt rs0 [R, L] (2 :: Int)
                rs2 = seq rs1 $ insertInt rs1 [L, R] (3 :: Int)
              in
                rs2
                    `shouldBe` [ ([], indirect [] 6)
                               , ([L], indirect [] 4)
                               , ([L, L], indirect [] 1)
                               , ([L, R], indirect [] 3)
                               , ([R], indirect [L] 2)
                               ]

        it "inserts 3 keys LL, LR, RL"
            $ let
                rs0 = insertInt [] [L, L] (1 :: Int)
                rs1 = insertInt rs0 [L, R] (2 :: Int)
                rs2 = insertInt rs1 [R, L] (3 :: Int)
              in
                rs2
                    `shouldBe` [ ([], indirect [] 6)
                               , ([L], indirect [] 3)
                               , ([R], indirect [L] 3)
                               , ([L, L], indirect [] 1)
                               , ([L, R], indirect [] 2)
                               ]

        it "inserting all leaves populates the full tree"
            $ forAll (elements [1 .. 10])
            $ \n -> forAll (genPaths n) $ \keys -> do
                let kvs = zip keys [1 :: Int .. 2 ^ n]
                inserted (+) kvs `shouldBe` summed n kvs
