{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StrictData #-}

module CSMT.Interface
    ( -- * Keys
      Direction (..)
    , Key
    , compareKeys
    , opposite

      -- * Interface Types
    , Indirect (..)
    , Insert
    , Query
    , CSMT (..)
    , fromBool
    , toBool
    )
where

-- | Key segment
data Direction = L | R deriving (Show, Eq, Ord)

-- | Convert Bool to Direction
fromBool :: Bool -> Direction
fromBool True = R
fromBool False = L

-- Convert Direction to its Bool representation
toBool :: Direction -> Bool
toBool L = False
toBool R = True

-- | Get the opposite direction
opposite :: Direction -> Direction
opposite L = R
opposite R = L

-- | Key type
type Key = [Direction]

-- | An indirect reference to a value stored at a given Key from a node
-- If the 'jump' key is empty then the value is stored at the current node
-- If the 'jump' key is non-empty then the value is stored at a descendant node
-- reachable by following the 'jump' key from the current node
data Indirect a = Indirect
    { jump :: Key
    , value :: a
    }
    deriving (Show, Eq, Functor)

-- | Type alias for an insert function in some monad m. It support batch inserts.
type Insert m a = [(Key, Indirect a)] -> m ()

-- | Type alias for a query function in some monad m.
type Query m a = Key -> m (Maybe (Indirect a))

-- | The backend interface for a CSMT in some monad m.
data CSMT m a = CSMT
    { insert :: Insert m a
    , query :: Query m a
    }

-- | Compare two keys and return their common prefix and the remaining suffixes
-- of each key after the common prefix.
compareKeys :: Key -> Key -> (Key, Key, Key)
compareKeys [] ys = ([], [], ys)
compareKeys xs [] = ([], xs, [])
compareKeys (x : xs) (y : ys)
    | x == y =
        let (j, o, r) = compareKeys xs ys
        in  (x : j, o, r)
    | otherwise = ([], x : xs, y : ys)
