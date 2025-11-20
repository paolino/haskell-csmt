{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module CSMT.DeletionSpec (spec)
where

import CSMT
    ( Direction (L, R)
    , Op (..)
    , pureCSMT
    , query
    , runPure
    )
import CSMT.Deletion
    ( DeletionPath (..)
    , deletionPathToOps
    , newDeletionPath
    )
import CSMT.Interface (Key)
import CSMT.Test.Lib
    ( deleteInt
    , genPaths
    , indirect
    , insertInt
    , inserted
    , intHashing
    , mkDeletionPath
    )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, elements, forAll, getSize, shuffle)

spec :: Spec
spec = do
    describe "deletion" $ do
        it "constructs a deletion path for a singleton tree"
            $ let
                rs0 = insertInt [] [] (1 :: Int)
                (mp, _) = runPure rs0 $ newDeletionPath (query pureCSMT) []
              in
                mp `shouldBe` Just (Value [] 1)
        it "constructs a deletion path for a tree with siblings"
            $ let
                rs0 = insertInt [] [L] (1 :: Int)
                rs1 = insertInt rs0 [R] (2 :: Int)
                mp = mkDeletionPath rs1 [L]
              in
                mp
                    `shouldBe` Just
                        (Branch [] L (Value [] 1) (indirect [] 2))
        it "constructs a deletion path for a tree with jumps"
            $ let
                rs0 = insertInt [] [L, L] (1 :: Int)
                rs1 = insertInt rs0 [L, R] (2 :: Int)
                mp = mkDeletionPath rs1 [L, L]
              in
                mp `shouldBe` Just (Branch [L] L (Value [] 1) (indirect [] 2))
        it "constructs a deletion path for a deeper tree with jumps"
            $ let
                rs0 = insertInt [] [L, L, R] (1 :: Int)
                rs1 = insertInt rs0 [L, R, L] (2 :: Int)
                rs2 = insertInt rs1 [R, L, R] (3 :: Int)
                mp0 = mkDeletionPath rs2 [L, L, R]
                mp1 = mkDeletionPath rs2 [L, R, L]
                mp2 = mkDeletionPath rs2 [R, L, R]
                rs3 = deleteInt rs2 [R, L, R]
              in
                do
                    mp0
                        `shouldBe` Just
                            ( Branch
                                []
                                L
                                ( Branch
                                    []
                                    L
                                    (Value [R] 1)
                                    (indirect [L] 2)
                                )
                                (indirect [L, R] 3)
                            )
                    mp1
                        `shouldBe` Just
                            ( Branch
                                []
                                L
                                ( Branch
                                    []
                                    R
                                    (Value [L] 2)
                                    (indirect [R] 1)
                                )
                                (indirect [L, R] 3)
                            )
                    mp2
                        `shouldBe` Just
                            ( Branch
                                []
                                R
                                (Value [L, R] 3)
                                (indirect [] 4)
                            )
                    rs3 `shouldBe` rs1

        it "constructs a deletion path for a deeper tree with jumps"
            $ let
                rs0 = insertInt [] [L, L, L] (1 :: Int)
                rs1 = insertInt rs0 [L, L, R] (2 :: Int)
                rs2 = insertInt rs1 [R, R, R] (3 :: Int)
                mp0 = mkDeletionPath rs2 [L, L, L]
                mp1 = mkDeletionPath rs2 [L, L, R]
                mp2 = mkDeletionPath rs2 [R, R, R]
              in
                do
                    mp0
                        `shouldBe` Just
                            ( Branch
                                []
                                L
                                ( Branch
                                    [L]
                                    L
                                    (Value [] 1)
                                    (indirect [] 2)
                                )
                                (indirect [R, R] 3)
                            )
                    mp1
                        `shouldBe` Just
                            ( Branch
                                []
                                L
                                ( Branch
                                    [L]
                                    R
                                    (Value [] 2)
                                    (indirect [] 1)
                                )
                                (indirect [R, R] 3)
                            )
                    mp2
                        `shouldBe` Just
                            ( Branch
                                []
                                R
                                (Value [R, R] 3)
                                (indirect [L] 3)
                            )

        it "deletes the singleton tree"
            $ let
                rs0 = insertInt [] [] (1 :: Int)
                rs1 = deleteInt rs0 []
              in
                rs1 `shouldBe` []
        it "deletes a single key"
            $ let
                rs0 = insertInt [] [L] (1 :: Int)
                rs1 = deleteInt rs0 [L]
              in
                rs1 `shouldBe` []
        it "deletes one of two sibling keys"
            $ let
                rs0 = insertInt [] [L] (1 :: Int)
                rs1 = insertInt rs0 [R] (2 :: Int)
                rs2 = deleteInt rs1 [R]
              in
                rs2 `shouldBe` rs0
        it "deletes one of four cousins keys"
            $ let
                rs0 = insertInt [] [L, L] (1 :: Int)
                rs1 = insertInt rs0 [L, R] (2 :: Int)
                rs2 = insertInt rs1 [R, L] (3 :: Int)
                rs3 = insertInt rs2 [R, R] (4 :: Int)
                dll = deleteInt rs3 [L, L]
                dlr = deleteInt rs3 [L, R]
                drl = deleteInt rs3 [R, L]
                drr = deleteInt rs3 [R, R]
              in
                do
                    dll
                        `shouldBe` [ ([], indirect [] 10)
                                   , ([R], indirect [] 7)
                                   , ([L], indirect [R] 2)
                                   , ([R, L], indirect [] 3)
                                   , ([R, R], indirect [] 4)
                                   ]
                    dlr
                        `shouldBe` [ ([], indirect [] 8)
                                   , ([R], indirect [] 7)
                                   , ([L], indirect [L] 1)
                                   , ([R, L], indirect [] 3)
                                   , ([R, R], indirect [] 4)
                                   ]
                    drl
                        `shouldBe` [ ([], indirect [] 8)
                                   , ([L], indirect [] 3)
                                   , ([R], indirect [R] 4)
                                   , ([L, L], indirect [] 1)
                                   , ([L, R], indirect [] 2)
                                   ]
                    drr
                        `shouldBe` [ ([], indirect [] 6)
                                   , ([L], indirect [] 3)
                                   , ([R], indirect [L] 3)
                                   , ([L, L], indirect [] 1)
                                   , ([L, R], indirect [] 2)
                                   ]
        it "deletes 2 of four cousin keys"
            $ let
                rs0 = insertInt [] [L, L] (1 :: Int)
                rs1 = insertInt rs0 [L, R] (2 :: Int)
                rs2 = insertInt rs1 [R, L] (3 :: Int)
                rs3 = insertInt rs2 [R, R] (4 :: Int)
                dlrl = deleteInt (deleteInt rs3 [L, L]) [R, R]
              in
                do
                    dlrl
                        `shouldBe` [ ([], indirect [] 6)
                                   , ([L], indirect [R] 2)
                                   , ([R], indirect [L] 3)
                                   ]
        it "deletes 3 of four cousin keys"
            $ let
                rs0 = insertInt [] [L, L] (1 :: Int)
                rs1 = insertInt rs0 [L, R] (2 :: Int)
                rs2 = insertInt rs1 [R, L] (3 :: Int)
                rs3 = insertInt rs2 [R, R] (4 :: Int)
                dll = deleteInt rs3 [L, L]
                drr = deleteInt dll [R, R]
                dlr = deleteInt drr [L, R]
              in
                do
                    dlr
                        `shouldBe` [ ([], indirect [R, L] 3)
                                   ]
        it "computes the right deletion path for [[L, R], [R, L]]"
            $ let
                rs0 = mempty
                rs1 = insertInt rs0 [L, R] (2 :: Int)
                rs2 = insertInt rs1 [R, L] (3 :: Int)
                mp = mkDeletionPath rs2 [L, R]
              in
                mp
                    `shouldBe` Just
                        ( Branch
                            []
                            L
                            (Value [R] 2)
                            (indirect [L] 3)
                        )
        it "computes the right ops to delete [L,R] from [[L, R], [R, L]]"
            $ let
                rs0 = mempty
                rs1 = insertInt rs0 [L, R] (2 :: Int)
                rs2 = insertInt rs1 [R, L] (3 :: Int)
                Just mp = mkDeletionPath rs2 [L, R]
                ops = deletionPathToOps intHashing mp
              in
                ops
                    `shouldBe` [ InsertCSMT [] (indirect [R, L] 3)
                               , DeleteCSMT [R]
                               , DeleteCSMT @() @() [L]
                               ]
        it
            "inserting some facts and deleting them in a random order results in an empty tree"
            $ forAll (elements [1 .. 19])
            $ \n -> forAll (randomPaths n) $ \(inserting, deleting) -> do
                let
                    kvs = zip inserting [1 :: Int .. 2 ^ n]
                    full = inserted intHashing kvs
                    emptied = foldl deleteInt full deleting
                emptied `shouldBe` []

randomPaths :: Int -> Gen ([Key], [Key])
randomPaths k = do
    xs <- genPaths k
    ys <- shuffle xs
    n <- getSize
    let ys' = take n ys
    zs <- shuffle ys'
    return (ys', zs)
