{-# LANGUAGE StrictData #-}

{- Types and functions for a standalone backend, where we do not need to access
other key-value stores. It's the one used in the app. For more complex library clients
these types will not suffice. -}
module CSMT.Backend.Standalone
    ( StandaloneOp
    , StandaloneCF (..)
    , Standalone (..)
    , StandaloneCodecs (..)
    , mkStandaloneOp
    )
where

import CSMT.Interface (Indirect (..), Key)
import Control.Lens (Prism', type (:~:) (..))
import Data.ByteString (ByteString)
import Data.GADT.Compare (GCompare (..), GEq (..), GOrdering (..))
import Database.KV.Transaction (KV)

data StandaloneCF = StandaloneKV | StandaloneCSMT

type StandaloneOp = (StandaloneCF, ByteString, Maybe ByteString)

mkStandaloneOp
    :: StandaloneCF -> ByteString -> Maybe ByteString -> StandaloneOp
mkStandaloneOp = (,,)

data Standalone k v a x where
    StandaloneKVCol :: Standalone k v a (KV k v)
    StandaloneCSMTCol :: Standalone k v a (KV Key (Indirect a))

instance GEq (Standalone k v a) where
    geq StandaloneKVCol StandaloneKVCol = Just Refl
    geq StandaloneCSMTCol StandaloneCSMTCol = Just Refl
    geq _ _ = Nothing

instance GCompare (Standalone k v a) where
    gcompare StandaloneKVCol StandaloneKVCol = GEQ
    gcompare StandaloneKVCol StandaloneCSMTCol = GLT
    gcompare StandaloneCSMTCol StandaloneKVCol = GGT
    gcompare StandaloneCSMTCol StandaloneCSMTCol = GEQ

data StandaloneCodecs k v a = StandaloneCodecs
    { keyCodec :: Prism' ByteString k
    , valueCodec :: Prism' ByteString v
    , nodeCodec :: Prism' ByteString a
    }
