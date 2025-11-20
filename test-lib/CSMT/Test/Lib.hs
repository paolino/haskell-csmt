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
    , emptyInMemoryDB
    , inserting
    , mkInclusionProof
    , pureBackend
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
import Data.ByteString (ByteString)
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
    :: Ord k
    => Hashing a
    -> InMemoryDB k v a
    -> Key
    -> a
    -> InMemoryDB k v a
insert hashing m k v = snd $ runPure m $ insertM hashing k v

delete
    :: Ord k => Hashing a -> InMemoryDB k v a -> Key -> InMemoryDB k v a
delete hashing m k = snd $ runPure m $ deleteM hashing k

deleteInt :: Ord k => InMemoryDB k v Int -> Key -> InMemoryDB k v Int
deleteInt = delete intHashing

insertInt
    :: InMemoryDB Int Int Int -> Key -> Int -> InMemoryDB Int Int Int
insertInt = insert intHashing

insertM :: Ord k => Hashing a -> Key -> a -> Pure k v a ()
insertM = inserting pureBackend

deleteM :: Ord k => Hashing a -> Key -> Pure k v a ()
deleteM = deleting pureBackend

insertMInt :: Key -> Int -> Pure Int Int Int ()
insertMInt = insertM intHashing

insertMHash :: Key -> Hash -> Pure ByteString ByteString Hash ()
insertMHash = insertM hashHashing

insertMList :: Key -> [Int] -> Pure [Int] [Int] [Int] ()
insertMList = insertM listHashing

keyToInt :: Key -> Int
keyToInt = foldl' (\acc d -> acc * 2 + dirToBit d) 0
  where
    dirToBit L = 0
    dirToBit R = 1

deleteMInt :: Ord k => Key -> Pure k v Int ()
deleteMInt = deleteM intHashing

proofM :: Ord k => Key -> Pure k v a (Maybe (Proof a))
proofM = mkInclusionProof pureBackend

verifyM :: (Eq a, Ord k) => Hashing a -> Key -> a -> Pure k v a Bool
verifyM hashing k v = do
    mp <- proofM k
    case mp of
        Nothing -> pure False
        Just p -> verifyInclusionProof pureBackend hashing v p

verifyMInt :: Ord k => Key -> Int -> Pure k v Int Bool
verifyMInt = verifyM intHashing

verifyMList :: Ord k => Key -> [Int] -> Pure k v [Int] Bool
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
verifyMHash :: Key -> Hash -> Pure ByteString ByteString Hash Bool
verifyMHash = verifyM hashHashing

indirect :: Key -> a -> Indirect a
indirect jump value = Indirect{jump, value}

intHash :: Int -> Hash
intHash = mkHash . fromString . show

inserted
    :: (Foldable t, Ord k)
    => Hashing a
    -> t (Key, a)
    -> InMemoryDB k v a
inserted hashing = foldl' (\m (k, v) -> insert hashing m k v) emptyInMemoryDB

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

mkDeletionPath
    :: Ord k => InMemoryDB k v a -> Key -> Maybe (DeletionPath a)
mkDeletionPath s = fst . runPure s . newDeletionPath (queryCSMT pureBackend)

-- showState :: Show a => Pure k v a ()
-- showState = do
--     s <- get
--     pTraceShow s $ pure ()

-- showProof :: Show a => Key -> Pure k v a ()
-- showProof k = do
--     p <- proofM k
--     pTraceShow p $ pure ()
