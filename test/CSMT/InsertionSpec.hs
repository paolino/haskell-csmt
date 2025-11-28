{-# LANGUAGE OverloadedLists #-}

module CSMT.InsertionSpec (spec)
where

import CSMT
    ( Direction (L, R)
    , InMemoryDB (inMemoryCSMT)
    , Indirect
    , Key
    , Pure
    , emptyInMemoryDB
    , inMemoryCSMT
    , runPure
    )
import CSMT.Test.Lib
    ( ListOf
    , element
    , insertMInt
    , list
    , node
    )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Prelude

hasExpectedDB
    :: (Show a, Eq a)
    => Pure Key v a b
    -> ListOf (Key, Indirect a) ()
    -> Expectation
hasExpectedDB program expectedDB =
    let (_, r) = runPure emptyInMemoryDB program
    in  inMemoryCSMT r `shouldBe` mkDb expectedDB

record :: Key -> Key -> Int -> ListOf (Key, Indirect Int) ()
record p k v = element (p, node k v)

i :: Key -> Int -> Pure Key Int Int ()
i = insertMInt

mkDb :: ListOf (Key, Indirect a) () -> Map Key (Indirect a)
mkDb = Map.fromList . list

spec :: Spec
spec = do
    describe "insertion with ints" $ do
        it "inserts 1 key L" $ do
            let p = i [L] 1
                db = record [] [L] 1
            p `hasExpectedDB` db
        it "inserts 1 key R" $ do
            let p = i [R] 1
                db = record [] [R] 1
            p `hasExpectedDB` db
        it "inserts 1 key LL" $ do
            let p = i [L, L] 1
                db = record [] [L, L] 1
            p `hasExpectedDB` db
        it "inserts 1 key LR" $ do
            let p = i [L, R] 1
                db = record [] [L, R] 1
            p `hasExpectedDB` db
        it "inserts 2 keys R and L" $ do
            let p = do
                    i [R] 2
                    i [L] 1
                db = do
                    record [] [] 3
                    record [L] [] 1
                    record [R] [] 2
            p `hasExpectedDB` db
        it "inserts 2 keys L and R" $ do
            let p = do
                    i [L] 1
                    i [R] 2
                db = do
                    record [] [] 3
                    record [L] [] 1
                    record [R] [] 2
             in p `hasExpectedDB` db
        it "inserts 2 keys LL and LR"
            $ let
                p = do
                    i [L, L] 1
                    i [L, R] 2
                db = do
                    record [] [L] 3
                    record [L, L] [] 1
                    record [L, R] [] 2
              in
                p `hasExpectedDB` db

        it "inserts 2 keys RR and LL" $ do
            let p = do
                    i [L, L] 1
                    i [R, R] 2
                db = do
                    record [] [] 4
                    record [L] [L] 1
                    record [R] [R] 2
            p `hasExpectedDB` db
        it "inserts 2 keys LR and RL" $ do
            let p = do
                    i [L, R] 1
                    i [R, L] 2
                db = do
                    record [] [] 4
                    record [L] [R] 1
                    record [R] [L] 2
            p `hasExpectedDB` db
        it "inserts 2 keys RR and RL and LL" $ do
            let
                p = do
                    i [R, L] 1
                    i [R, R] 2
                    i [L, L] 3
                db = do
                    record [] [] 6
                    record [R] [] 3
                    record [R, L] [] 1
                    record [R, R] [] 2
                    record [L] [L] 3
            p `hasExpectedDB` db
        it "inserts 3 keys LL, RL, LR" $ do
            let p = do
                    i [L, L] 1
                    i [R, L] 2
                    i [L, R] 3
                db = do
                    record [] [] 6
                    record [L] [] 4
                    record [L, L] [] 1
                    record [L, R] [] 3
                    record [R] [L] 2
            p `hasExpectedDB` db
        it "inserts 3 keys LL, LR, RL" $ do
            let p = do
                    i [L, L] 1
                    i [L, R] 2
                    i [R, L] 3
                db = do
                    record [] [] 6
                    record [L] [] 3
                    record [R] [L] 3
                    record [L, L] [] 1
                    record [L, R] [] 2
            p `hasExpectedDB` db

        it "cover the docs example" $ do
            let p = do
                    i [L, L, R, R] 13
                    i [L, R, R, L] 5
                    i [L, R, R, R] 19
                    i [R, L, R, L] 23
                db = do
                    record [] [] 66
                    record [L] [] 41
                    record [L, R] [R] 24
                    record [L, L] [R, R] 13
                    record [L, R, R, L] [] 5
                    record [L, R, R, R] [] 19
                    record [R] [L, R, L] 23
            p `hasExpectedDB` db
