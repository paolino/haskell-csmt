{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedLists #-}

module CSMT.Test.Lib
    ( delete
    , deleteInt
    , deleteM
    , deleteMInt
    , genKey
    , genPaths
    , genSomePaths
    , node
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
    , identityFromKV
    , element
    , list
    , ListOf
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
import CSMT.Interface (FromKV (..), Hashing (..))
import Control.Monad.Free (Free (..), liftF)
import Data.Foldable (Foldable (..), foldl')
import Data.List (nub)
import Data.String (IsString (..))
import Test.QuickCheck
    ( listOf
    , listOf1
    , shuffle
    )
import Test.QuickCheck.Gen (Gen, elements)

identityFromKV :: FromKV Key a a
identityFromKV = FromKV{fromK = id, fromV = id}

intHashing :: Hashing Int
intHashing =
    Hashing
        { rootHash = \(Indirect k v) -> keyToInt k + v
        , combineHash = \(Indirect kl l) (Indirect kr r) ->
            keyToInt kl + keyToInt kr + l + r
        }
insert
    :: Ord k
    => FromKV k v a
    -> Hashing a
    -> InMemoryDB k v a
    -> k
    -> v
    -> InMemoryDB k v a
insert fromKV hashing m k v = snd $ runPure m $ insertM fromKV hashing k v

delete
    :: Ord k
    => FromKV k v a
    -> Hashing a
    -> InMemoryDB k v a
    -> k
    -> InMemoryDB k v a
delete fromKV hashing m k = snd $ runPure m $ deleteM fromKV hashing k

deleteInt :: InMemoryDB Key Int Int -> Key -> InMemoryDB Key Int Int
deleteInt = delete identityFromKV intHashing

insertInt
    :: InMemoryDB Key Int Int -> Key -> Int -> InMemoryDB Key Int Int
insertInt = insert identityFromKV intHashing

insertM
    :: Ord k => FromKV k v a -> Hashing a -> k -> v -> Pure k v a ()
insertM = inserting . pureBackend

deleteM :: Ord k => FromKV k v a -> Hashing a -> k -> Pure k v a ()
deleteM = deleting . pureBackend

insertMInt :: Key -> Int -> Pure Key Int Int ()
insertMInt = insertM FromKV{fromK = id, fromV = id} intHashing

insertMHash :: Key -> Hash -> Pure Key Hash Hash ()
insertMHash = insertM FromKV{fromK = id, fromV = id} hashHashing

insertMList :: Key -> [Int] -> Pure Key [Int] [Int] ()
insertMList = insertM FromKV{fromK = id, fromV = id} listHashing

keyToInt :: Key -> Int
keyToInt = foldl' (\acc d -> acc * 2 + dirToBit d) 0
  where
    dirToBit L = 0
    dirToBit R = 1

deleteMInt :: Key -> Pure Key Int Int ()
deleteMInt = deleteM FromKV{fromK = id, fromV = id} intHashing

proofM :: Ord k => FromKV k v a -> k -> Pure k v a (Maybe (Proof a))
proofM = mkInclusionProof . pureBackend

verifyM
    :: (Eq a, Ord k)
    => FromKV k v a
    -> Hashing a
    -> k
    -> v
    -> Pure k v a Bool
verifyM fromKV hashing k v = do
    mp <- proofM fromKV k
    case mp of
        Nothing -> pure False
        Just p -> verifyInclusionProof (pureBackend fromKV) hashing v p

verifyMInt :: Key -> Int -> Pure Key Int Int Bool
verifyMInt = verifyM FromKV{fromK = id, fromV = id} intHashing

verifyMList :: Key -> [Int] -> Pure Key [Int] [Int] Bool
verifyMList = verifyM FromKV{fromK = id, fromV = id} listHashing

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
verifyMHash :: Key -> Hash -> Pure Key Hash Hash Bool
verifyMHash = verifyM FromKV{fromK = id, fromV = id} hashHashing

node :: Key -> a -> Indirect a
node jump value = Indirect{jump, value}

intHash :: Int -> Hash
intHash = mkHash . fromString . show

inserted
    :: (Foldable t, Ord k)
    => FromKV k v a
    -> Hashing a
    -> t (k, v)
    -> InMemoryDB k v a
inserted fromKV hashing = foldl' (\m (k, v) -> insert fromKV hashing m k v) emptyInMemoryDB
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
    :: Ord k
    => FromKV k v a
    -> InMemoryDB k v a
    -> Key
    -> Maybe (DeletionPath a)
mkDeletionPath fromKV s =
    fst
        . runPure s
        . newDeletionPath (queryCSMT $ pureBackend fromKV)

data List e a
    = Cons e a
    deriving (Functor)

type ListOf e = Free (List e)

element :: e -> ListOf e ()
element x = liftF (Cons x ())

list :: ListOf a () -> [a]
list (Pure _) = []
list (Free (Cons x xs)) = x : list xs
