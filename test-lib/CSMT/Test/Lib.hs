{-# LANGUAGE OverloadedLists #-}

module CSMT.Test.Lib
    ( delete
    , deleteInt
    , deleteM
    , deleteMInt
    , genKey
    , genPaths
    , genSomePaths
    , indirect
    , insert
    , inserted
    , insertInt
    , insertM
    , insertMInt
    , intHash
    , intHashing
    , mkDeletionPath
    , proofM
    -- , summed
    , verifyM
    , verifyMHash
    , verifyMInt
    , verifyMList
    , listHashing
    , keyToListOfInt
    , insertMHash
    , insertMList
    )
where

import CSMT
    ( Direction (L, R)
    , Hashing
    , InMemoryDB
    , Indirect (..)
    , Key
    , Proof
    , Pure
    , inserting
    , mkInclusionProof
    , pureCSMT
    , queryCSMT
    , runPure
    , verifyInclusionProof
    )
import CSMT.Deletion
    ( DeletionPath (..)
    , deleting
    , newDeletionPath
    )
import CSMT.Hashes (Hash, hashHashing, mkHash)
import CSMT.Interface (Hashing (..))
import Data.Foldable (Foldable (..), foldl')
import Data.List (nub)
import Data.String (IsString (..))
import Test.QuickCheck
    ( listOf
    , listOf1
    , shuffle
    )
import Test.QuickCheck.Gen (Gen, elements)

intHashing :: Hashing Int
intHashing =
    Hashing
        { rootHash = \(Indirect k v) -> keyToInt k + v
        , combineHash = \(Indirect kl l) (Indirect kr r) ->
            keyToInt kl + keyToInt kr + l + r
        }
insert
    :: Hashing a
    -> InMemoryDB a
    -> Key
    -> a
    -> InMemoryDB a
insert hashing m k v = snd $ runPure m $ insertM hashing k v

delete
    :: Hashing a -> InMemoryDB a -> Key -> InMemoryDB a
delete hashing m k = snd $ runPure m $ deleteM hashing k

deleteInt :: InMemoryDB Int -> Key -> InMemoryDB Int
deleteInt = delete intHashing

insertInt :: InMemoryDB Int -> Key -> Int -> InMemoryDB Int
insertInt = insert intHashing

insertM :: Hashing a -> Key -> a -> Pure a ()
insertM = inserting pureCSMT

deleteM :: Hashing a -> Key -> Pure a ()
deleteM = deleting pureCSMT

insertMInt :: Key -> Int -> Pure Int ()
insertMInt = insertM intHashing

insertMHash :: Key -> Hash -> Pure Hash ()
insertMHash = insertM hashHashing

insertMList :: Key -> [Int] -> Pure [Int] ()
insertMList = insertM listHashing

keyToInt :: Key -> Int
keyToInt = foldl' (\acc d -> acc * 2 + dirToBit d) 0
  where
    dirToBit L = 0
    dirToBit R = 1

deleteMInt :: Key -> Pure Int ()
deleteMInt = deleteM intHashing

proofM :: Key -> Pure a (Maybe (Proof a))
proofM = mkInclusionProof pureCSMT

verifyM :: Eq a => Hashing a -> Key -> a -> Pure a Bool
verifyM hashing k v = do
    mp <- proofM k
    case mp of
        Nothing -> pure False
        Just p -> verifyInclusionProof pureCSMT hashing v p

verifyMInt :: Key -> Int -> Pure Int Bool
verifyMInt = verifyM intHashing

verifyMList :: Key -> [Int] -> Pure [Int] Bool
verifyMList = verifyM listHashing

keyToListOfInt :: Key -> [Int]
keyToListOfInt [] = [-1]
keyToListOfInt xs = flip fmap xs $ \case
    L -> 0
    R -> 1

listHashing :: Hashing [Int]
listHashing =
    Hashing
        { rootHash = \(Indirect k v) -> keyToListOfInt k <> v
        , combineHash = \(Indirect kl l) (Indirect kr r) ->
            keyToListOfInt kl <> l <> keyToListOfInt kr <> r
        }
verifyMHash :: Key -> Hash -> Pure Hash Bool
verifyMHash = verifyM hashHashing

indirect :: Key -> a -> Indirect a
indirect jump value = Indirect{jump, value}

intHash :: Int -> Hash
intHash = mkHash . fromString . show

inserted
    :: Foldable t
    => Hashing a
    -> t (Key, a)
    -> InMemoryDB a
inserted hashing = foldl' (\m (k, v) -> insert hashing m k v) []

-- summed :: Int -> [(Key, Int)] -> Map Key (Indirect Int)
-- summed n kvs =
--     Map.fromList $ allInits n <&> \x ->
--         let
--             w = (Map.fromList (toList kvs) Map.!)
--         in
--             ( x
--             , indirect []
--                 $ foldl' (+) 0
--                 $ NE.fromList
--                 $ fmap w
--                 $ filter (isPrefixOf x)
--                 $ allPaths n
--             )
--   where
--     allInits :: Int -> [Key]
--     allInits 0 = [[]]
--     allInits c =
--         allInits (c - 1) <> do
--             p <- allInits (c - 1)
--             [p, L : p, R : p]

allPaths :: Int -> [Key]
allPaths 0 = [[]]
allPaths c = do
    p <- allPaths (c - 1)
    [L : p, R : p]

genKey :: Gen Key
genKey = listOf $ elements [L, R]

genPaths :: Int -> Gen [Key]
genPaths n = shuffle (allPaths n)

genSomePaths :: Int -> Gen [Key]
genSomePaths n = fmap nub <$> listOf1 $ do
    let go 0 = return []
        go c = do
            d <- elements [L, R]
            ds <- go (c - 1)
            return (d : ds)
    go n

mkDeletionPath :: InMemoryDB a -> Key -> Maybe (DeletionPath a)
mkDeletionPath s = fst . runPure s . newDeletionPath (queryCSMT pureCSMT)

-- showState :: Show a => Pure a ()
-- showState = do
--     s <- get
--     pTraceShow s $ pure ()

-- showProof :: Show a => Key -> Pure a ()
-- showProof k = do
--     p <- proofM k
--     pTraceShow p $ pure ()
