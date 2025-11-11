{-# LANGUAGE OverloadedLists #-}

module SMTSpec (spec)
where

import Data.Map.Strict (Map)
import SMT
    ( Direction (..)
    , Key
    , Proof
    , Pure
    , inserting
    , mkProof
    , pureInsert
    , pureQuery
    , runPure
    , verifyProof
    )
import Test.Hspec (Spec, describe, it, shouldBe)

mk :: Map Key Int -> Key -> Int -> Map Key Int
mk m k v = snd $ runPure m $ mkM k v

mkM :: Key -> Int -> Pure Int ()
mkM = inserting (pureQuery 0) pureInsert (+)

pfM :: Key -> Pure Int (Maybe (Proof Int))
pfM = mkProof (pureQuery 0)

vpfM :: Key -> Int -> Pure Int Bool
vpfM k v = do
    mp <- pfM k
    case mp of
        Nothing -> pure False
        Just p -> verifyProof (pureQuery 0) (+) v p

spec :: Spec
spec = do
    describe "insertKV" $ do
        it "inserts a left key"
            $ let
                rs = mk [] [L] (1 :: Int)
              in
                rs `shouldBe` [([L], 1), ([R], 0), ([], 1)]
        it "inserts a right key"
            $ let
                rs = mk [] [R] (1 :: Int)
              in
                rs `shouldBe` [([L], 0), ([R], 1), ([], 1)]
        it "inserts a deeper left key"
            $ let
                rs = mk [] [L, L] (1 :: Int)
              in
                rs
                    `shouldBe` [ ([L, L], 1)
                               , ([L, R], 0)
                               , ([R], 0)
                               , ([], 1)
                               , ([L], 1)
                               ]
        it "inserts a deeper right key"
            $ let
                rs = mk [] [R, R] (1 :: Int)
              in
                rs
                    `shouldBe` [ ([L], 0)
                               , ([R, L], 0)
                               , ([R, R], 1)
                               , ([], 1)
                               , ([R], 1)
                               ]
        it "inserts a key with existing sibling"
            $ let
                rs1 = mk [] [L, L] (1 :: Int)
                rs2 = mk rs1 [R, R] (2 :: Int)
                rs3 = mk rs2 [R, L] (3 :: Int)
                rs4 = mk rs3 [L, R] (4 :: Int)
              in
                do
                    rs1
                        `shouldBe` [ ([L, L], 1)
                                   , ([L, R], 0)
                                   , ([R], 0)
                                   , ([], 1)
                                   , ([L], 1)
                                   ]
                    rs2
                        `shouldBe` [ ([L, L], 1)
                                   , ([L, R], 0)
                                   , ([R, R], 2)
                                   , ([R, L], 0)
                                   , ([], 3)
                                   , ([L], 1)
                                   , ([R], 2)
                                   ]
                    rs3
                        `shouldBe` [ ([L, L], 1)
                                   , ([L, R], 0)
                                   , ([R, R], 2)
                                   , ([R, L], 3)
                                   , ([], 6)
                                   , ([L], 1)
                                   , ([R], 5)
                                   ]
                    rs4
                        `shouldBe` [ ([L, L], 1)
                                   , ([L, R], 4)
                                   , ([R, R], 2)
                                   , ([R, L], 3)
                                   , ([], 10)
                                   , ([L], 5)
                                   , ([R], 5)
                                   ]
    describe "Sparse Merkle Tree proof" $ do
        it "verifies a simple fact"
            $ let (r, _m) = runPure [] $ do
                    mkM [L] (1 :: Int)
                    vpfM [L] 1
              in  r `shouldBe` True
        it "verifies a deeper fact"
            $ let (r, _m) = runPure [] $ do
                    mkM [L, R, L] (42 :: Int)
                    vpfM [L, R, L] 42
              in  r `shouldBe` True
        it "verifies a fact with siblings"
            $ let (r, _m) = runPure [] $ do
                    mkM [L] (10 :: Int)
                    mkM [R] (20 :: Int)
                    vpfM [L] 10
              in  r `shouldBe` True
        it "verifies another fact with siblings"
            $ let (r, _m) = runPure [] $ do
                    mkM [L, L] (5 :: Int)
                    mkM [L, R] (15 :: Int)
                    mkM [R, L] (25 :: Int)
                    mkM [R, R] (35 :: Int)
                    vpfM [R, L] 25
              in  r `shouldBe` True

-- fact :: Gen (ByteString, ByteString)
-- fact = do
--     k <- B.pack <$> listOf asciiChar
--     v <- B.pack <$> listOf asciiChar
--     return (k, v)

-- asciiChar :: Gen Word8
-- asciiChar = choose (32, 126) -- Printable ASCII range

-- facts :: Gen [(ByteString, ByteString)]
-- facts = listOf fact
-- describe "Sparse Merkle Tree" $ do
-- it "inserts and retrieves singleton root correctly" $ do
--     let smt  = insert "key1" "value1" emptySMT
--     root smt `shouldBe` Just (mkHash "value1")
-- it "inserts and retrieves multiple keys correctly" $ do
--     let smt1 = insert "key1" "value1" emptySMT
--     let smt2 = insert "key2" "value2" smt1
--     root smt2 `shouldBe` Just (mkHash $ B.concat
--         [ mkHash "value1"
--         , mkHash "value2"
--         , mkHash "value3"
--         ])
-- it "inserts and builds proof correctly"
--     $ property
--     $ forAllShrink
--         facts
--         (pure . drop 1)
--     $ \kvs ->
--         let
--             smt = foldl' (flip $ uncurry insert) emptySMT kvs
--             check (k, v) =
--                 case buildProof k smt of
--                     Nothing -> False
--                     Just proof ->
--                         let rootHash = applyProof v proof
--                         in  case root smt of
--                                 Nothing -> False
--                                 Just rh -> rh == rootHash
--         in
--             all check kvs