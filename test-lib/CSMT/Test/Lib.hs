{-# LANGUAGE OverloadedLists #-}

module CSMT.Test.Lib
    ( insert
    , insertInt
    , insertM
    , insertMInt
    , delete
    , deleteInt
    , deleteM
    , deleteMInt
    , proofM
    , verifyM
    , verifyMInt
    , verifyMHash
    , indirect
    , intHash
    , inserted
    , summed
    , genPaths
    , genSomePaths
    , mkDeletionPath
    )
where

import CSMT
    ( Direction (L, R)
    , InMemoryDB
    , Indirect (..)
    , Key
    , Proof
    , Pure
    , inserting
    , mkInclusionProof
    , pureCSMT
    , query
    , runPure
    , verifyInclusionProof
    )
import CSMT.Deletion
    ( DeletionPath (..)
    , deleting
    , newDeletionPath
    )
import CSMT.Hashes (Hash, addHash, mkHash)
import Data.Foldable (Foldable (..), foldl')
import Data.Functor ((<&>))
import Data.List (isPrefixOf, nub)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.String (IsString (..))
import Test.QuickCheck
    ( listOf1
    , shuffle
    )
import Test.QuickCheck.Gen (Gen, elements)

insert :: (a -> a -> a) -> InMemoryDB a -> Key -> a -> InMemoryDB a
insert a m k v = snd $ runPure m $ insertM a k v

delete :: (a -> a -> a) -> InMemoryDB a -> Key -> InMemoryDB a
delete a m k = snd $ runPure m $ deleteM a k

deleteInt :: InMemoryDB Int -> Key -> InMemoryDB Int
deleteInt = delete (+)

insertInt :: InMemoryDB Int -> Key -> Int -> InMemoryDB Int
insertInt = insert (+)

insertM :: (a -> a -> a) -> Key -> a -> Pure a ()
insertM = inserting pureCSMT

deleteM :: (a -> a -> a) -> Key -> Pure a ()
deleteM = deleting pureCSMT

insertMInt :: Key -> Int -> Pure Int ()
insertMInt = insertM (+)

deleteMInt :: Key -> Pure Int ()
deleteMInt = deleteM (+)

proofM :: Key -> Pure a (Maybe (Proof a))
proofM = mkInclusionProof pureCSMT

verifyM :: Eq a => (a -> a -> a) -> Key -> a -> Pure a Bool
verifyM a k v = do
    mp <- proofM k
    case mp of
        Nothing -> pure False
        Just p -> verifyInclusionProof pureCSMT a v p

verifyMInt :: Key -> Int -> Pure Int Bool
verifyMInt = verifyM (+)

verifyMHash :: Key -> Hash -> Pure Hash Bool
verifyMHash = verifyM addHash

indirect :: Key -> a -> Indirect a
indirect p v = Indirect{jump = p, value = v}

intHash :: Int -> Hash
intHash = mkHash . fromString . show

inserted :: Foldable t => (a -> a -> a) -> t (Key, a) -> InMemoryDB a
inserted a = foldl' (\m (k, v) -> insert a m k v) []

summed :: Int -> [(Key, Int)] -> Map Key (Indirect Int)
summed n kvs =
    Map.fromList $ allInits n <&> \x ->
        let
            w = (Map.fromList (toList kvs) Map.!)
        in
            ( x
            , indirect []
                $ foldl' (+) 0
                $ NE.fromList
                $ fmap w
                $ filter (isPrefixOf x)
                $ allPaths n
            )
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

genSomePaths :: Int -> Gen [Key]
genSomePaths n = fmap nub <$> listOf1 $ do
    let go 0 = return []
        go c = do
            d <- elements [L, R]
            ds <- go (c - 1)
            return (d : ds)
    go n

mkDeletionPath :: InMemoryDB a -> Key -> Maybe (DeletionPath a)
mkDeletionPath s = fst . runPure s . newDeletionPath (query pureCSMT)