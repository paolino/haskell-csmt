{-# LANGUAGE OverloadedLists #-}

module CSMTSpec (spec)
where

import CSMT
    ( Direction (L, R)
    , Indirect (..)
    , Key
    , Pure
    , compareKeys
    , inserting
    , pureCSMT
    , runPure
    )
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (forAll, shuffle)
import Test.QuickCheck.Gen (Gen, elements)

mk :: Map Key (Indirect Int) -> Key -> Int -> Map Key (Indirect Int)
mk m k v = snd $ runPure m $ mkM k v

mkM :: Key -> Int -> Pure Int ()
mkM = inserting pureCSMT (+)

-- pfM :: Key -> Pure Int (Maybe (Proof Int))
-- pfM = mkProof (pureQuery 0)

-- vpfM :: Key -> Int -> Pure Int Bool
-- vpfM k v = do
--     mp <- pfM k
--     case mp of
--         Nothing -> pure False
--         Just p -> verifyProof (pureQuery 0) (+) v p

i :: Key -> a -> Indirect a
i p v = Indirect{jump = p, value = v}

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
    describe "inserting" $ do
        it "inserts 1 key L"
            $ let
                rs = mk [] [L] (1 :: Int)
              in
                rs `shouldBe` [([], i [L] 1)]
        it "inserts 1 key R"
            $ let
                rs = mk [] [R] (1 :: Int)
              in
                rs `shouldBe` [([], i [R] 1)]
        it "inserts 1 key LL"
            $ let
                rs = mk [] [L, L] (1 :: Int)
              in
                rs `shouldBe` [([], i [L, L] 1)]
        it "inserts 1 key LR"
            $ let
                rs = mk [] [R, R] (1 :: Int)
              in
                rs `shouldBe` [([], i [R, R] 1)]
        it "inserts 2 keys R and L"
            $ let
                rs0 = mk [] [L] (1 :: Int)
                rs1 = mk rs0 [R] (2 :: Int)
              in
                rs1
                    `shouldBe` [ ([], i [] 3)
                               , ([L], i [] 1)
                               , ([R], i [] 2)
                               ]
        it "inserts 2 keys L and R"
            $ let
                rs0 = mk [] [R] (2 :: Int)
                rs1 = mk rs0 [L] (1 :: Int)
              in
                rs1
                    `shouldBe` [ ([], i [] 3)
                               , ([L], i [] 1)
                               , ([R], i [] 2)
                               ]
        it "inserts 2 keys LL and LR"
            $ let
                rs0 = mk [] [L, L] (1 :: Int)
                rs1 = mk rs0 [L, R] (2 :: Int)
              in
                rs1
                    `shouldBe` [ ([], i [L] 3)
                               , ([L, L], i [] 1)
                               , ([L, R], i [] 2)
                               ]

        it "inserts 2 keys RR and LL"
            $ let
                rs0 = mk [] [L, L] (1 :: Int)
                rs1 = mk rs0 [R, R] (2 :: Int)
              in
                rs1
                    `shouldBe` [ ([], i [] 3)
                               , ([L], i [L] 1)
                               , ([R], i [R] 2)
                               ]
        it "inserts 2 keys LR and RL"
            $ let
                rs0 = mk [] [L, R] (1 :: Int)
                rs1 = mk rs0 [R, L] (2 :: Int)
              in
                rs1
                    `shouldBe` [ ([], i [] 3)
                               , ([L], i [R] 1)
                               , ([R], i [L] 2)
                               ]
        it "inserts 3 keys LL, RL, LR"
            $ let
                rs0 = mk [] [L, L] (1 :: Int)
                rs1 = seq rs0 $ mk rs0 [R, L] (2 :: Int)
                rs2 = seq rs1 $ mk rs1 [L, R] (3 :: Int)
              in
                rs2
                    `shouldBe` [ ([], i [] 6)
                               , ([L], i [] 4)
                               , ([L, L], i [] 1)
                               , ([L, R], i [] 3)
                               , ([R], i [L] 2)
                               ]

        it "inserts 3 keys LL, LR, RL"
            $ let
                rs0 = mk [] [L, L] (1 :: Int)
                rs1 = mk rs0 [L, R] (2 :: Int)
                rs2 = mk rs1 [R, L] (3 :: Int)
              in
                rs2
                    `shouldBe` [ ([], i [] 6)
                               , ([L], i [] 3)
                               , ([R], i [L] 3)
                               , ([L, L], i [] 1)
                               , ([L, R], i [] 2)
                               ]

        it "inserting all cover the full tree"
            $ forAll (elements [1 .. 10])
            $ \n -> forAll (genPaths n) $ \keys -> do
                let kvs = zip keys [1 .. 2 ^ n]
                inserted kvs `shouldBe` summed n kvs

inserted :: [(Key, Int)] -> Map Key (Indirect Int)
inserted = foldl' (\m (k, v) -> mk m k v) []

summed :: Int -> [(Key, Int)] -> Map Key (Indirect Int)
summed n kvs =
    Map.fromList $ allInits n <&> \x ->
        let
            w = (Map.fromList kvs Map.!)
        in
            (x, i [] $ sum [w p | p <- allPaths n, x `isPrefixOf` p])
  where
    allInits :: Int -> [Key]
    allInits 0 = [[]]
    allInits c =
        allInits (c - 1) <> do
            p <- allInits (c - 1)
            [p, L : p, R : p]

allPaths :: Int -> [Key]
allPaths 0 = [[]]
allPaths c = do
    p <- allPaths (c - 1)
    [L : p, R : p]

genPaths :: Int -> Gen [Key]
genPaths n = shuffle (allPaths n)

-- describe "Sparse Merkle Tree proof" $ do
--     it "verifies a simple fact"
--         $ let (r, _m) = runPure [] $ do
--                 mkM [L] (1 :: Int)
--                 vpfM [L] 1
--           in  r `shouldBe` True
--     it "verifies a deeper fact"
--         $ let (r, _m) = runPure [] $ do
--                 mkM [L, R, L] (42 :: Int)
--                 vpfM [L, R, L] 42
--           in  r `shouldBe` True
--     it "verifies a fact with siblings"
--         $ let (r, _m) = runPure [] $ do
--                 mkM [L] (10 :: Int)
--                 mkM [R] (20 :: Int)
--                 vpfM [L] 10
--           in  r `shouldBe` True
--     it "verifies another fact with siblings"
--         $ let (r, _m) = runPure [] $ do
--                 mkM [L, L] (5 :: Int)
--                 mkM [L, R] (15 :: Int)
--                 mkM [R, L] (25 :: Int)
--                 mkM [R, R] (35 :: Int)
--                 vpfM [R, L] 25
--           in  r `shouldBe` True

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
