{-# LANGUAGE OverloadedLists #-}

module CSMT.InsertionSpec (spec)
where

import CSMT
    ( Direction (L, R)
    , InMemoryDB (inMemoryCSMT)
    , emptyInMemoryDB
    , inMemoryCSMT
    , mkCompose
    , pureBackend
    , queryCSMT
    , runPure
    )
import CSMT.Test.Lib
    ( indirect
    , insertInt
    )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "insertion" $ do
        it "inserts 1 key L"
            $ let
                rs = insertInt emptyInMemoryDB [L] (1 :: Int)
              in
                inMemoryCSMT rs `shouldBe` [([], indirect [L] 1)]
        it "inserts 1 key R"
            $ let
                rs = insertInt emptyInMemoryDB [R] (1 :: Int)
              in
                inMemoryCSMT rs `shouldBe` [([], indirect [R] 1)]
        it "inserts 1 key LL"
            $ let
                rs = insertInt emptyInMemoryDB [L, L] (1 :: Int)
              in
                inMemoryCSMT rs `shouldBe` [([], indirect [L, L] 1)]
        it "inserts 1 key LR"
            $ let
                rs = insertInt emptyInMemoryDB [R, R] (1 :: Int)
              in
                inMemoryCSMT rs `shouldBe` [([], indirect [R, R] 1)]
        it "inserts 2 keys R and L"
            $ let
                rs0 = insertInt emptyInMemoryDB [L] (1 :: Int)
                rs1 = insertInt rs0 [R] (2 :: Int)
              in
                inMemoryCSMT rs1
                    `shouldBe` [ ([], indirect [] 3)
                               , ([L], indirect [] 1)
                               , ([R], indirect [] 2)
                               ]
        it "inserts 2 keys L and R"
            $ let
                rs0 = insertInt emptyInMemoryDB [R] (2 :: Int)
                rs1 = insertInt rs0 [L] (1 :: Int)
              in
                inMemoryCSMT rs1
                    `shouldBe` [ ([], indirect [] 3)
                               , ([L], indirect [] 1)
                               , ([R], indirect [] 2)
                               ]
        it "inserts 2 keys LL and LR"
            $ let
                rs0 = insertInt emptyInMemoryDB [L, L] (1 :: Int)
                rs1 = insertInt rs0 [L, R] (2 :: Int)
              in
                inMemoryCSMT rs1
                    `shouldBe` [ ([], indirect [L] 3)
                               , ([L, L], indirect [] 1)
                               , ([L, R], indirect [] 2)
                               ]

        it "inserts 2 keys RR and LL"
            $ let
                rs0 = insertInt emptyInMemoryDB [L, L] (1 :: Int)
                rs1 = insertInt rs0 [R, R] (2 :: Int)
              in
                inMemoryCSMT rs1
                    `shouldBe` [ ([], indirect [] 4)
                               , ([L], indirect [L] 1)
                               , ([R], indirect [R] 2)
                               ]
        it "inserts 2 keys LR and RL"
            $ let
                rs0 = insertInt emptyInMemoryDB [L, R] (1 :: Int)
                rs1 = insertInt rs0 [R, L] (2 :: Int)
              in
                do
                    inMemoryCSMT rs1
                        `shouldBe` [ ([], indirect [] 4)
                                   , ([L], indirect [R] 1)
                                   , ([R], indirect [L] 2)
                                   ]
        it "inserts 2 keys RR and RL and LL"
            $ let
                rs0 = insertInt emptyInMemoryDB [R, L] (1 :: Int)
                rs1 = insertInt rs0 [R, R] (2 :: Int)
                _d12 =
                    runPure rs1
                        $ mkCompose
                            (queryCSMT pureBackend)
                            [L, L]
                            3
                rs2 = seq rs1 $ insertInt rs1 [L, L] (3 :: Int)
              in
                do
                    inMemoryCSMT rs2
                        `shouldBe` [ ([], indirect [] 6)
                                   , ([R], indirect [] 3)
                                   , ([R, L], indirect [] 1)
                                   , ([R, R], indirect [] 2)
                                   , ([L], indirect [L] 3)
                                   ]
        it "inserts 3 keys LL, RL, LR"
            $ let
                rs0 = insertInt emptyInMemoryDB [L, L] (1 :: Int)
                rs1 = seq rs0 $ insertInt rs0 [R, L] (2 :: Int)
                rs2 = seq rs1 $ insertInt rs1 [L, R] (3 :: Int)
              in
                inMemoryCSMT rs2
                    `shouldBe` [ ([], indirect [] 6)
                               , ([L], indirect [] 4)
                               , ([L, L], indirect [] 1)
                               , ([L, R], indirect [] 3)
                               , ([R], indirect [L] 2)
                               ]

        it "inserts 3 keys LL, LR, RL"
            $ let
                rs0 = insertInt emptyInMemoryDB [L, L] (1 :: Int)
                rs1 = insertInt rs0 [L, R] (2 :: Int)
                rs2 = insertInt rs1 [R, L] (3 :: Int)
              in
                inMemoryCSMT rs2
                    `shouldBe` [ ([], indirect [] 6)
                               , ([L], indirect [] 3)
                               , ([R], indirect [L] 3)
                               , ([L, L], indirect [] 1)
                               , ([L, R], indirect [] 2)
                               ]
