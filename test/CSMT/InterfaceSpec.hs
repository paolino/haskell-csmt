{-# LANGUAGE OverloadedLists #-}

module CSMT.InterfaceSpec (spec)
where

import CSMT
    ( Direction (L, R)
    , compareKeys
    )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    describe "compareKeys" $ do
        it "handles empty keys"
            $ compareKeys [] []
            `shouldBe` ([], [], [])
        it "handles identical keys"
            $ compareKeys [L, R, L] [L, R, L]
            `shouldBe` ([L, R, L], [], [])
        it "handles common prefixes"
            $ compareKeys [L, R, R, R] [L, R, L, R]
            `shouldBe` ([L, R], [R, R], [L, R])
