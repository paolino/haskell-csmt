{-# LANGUAGE StrictData #-}

{- Types and functions for a pure in-memory backend for CSMT, useful for testing.delete
We target directly the standalone backend types and functions as this is not intended for
library use.
 -}

module CSMT.Backend.Pure
    ( InMemoryDB (..)
    , Pure
    , standalonePureCols
    , runPure
    , emptyInMemoryDB
    , pureDatabase
    , inMemoryCSMTParsed
    , runPureTransaction
    )
where

import CSMT.Backend.Standalone
    ( Standalone (..)
    , StandaloneCF (..)
    , StandaloneCodecs (..)
    , StandaloneOp
    , mkStandaloneOp
    )
import CSMT.Interface
    ( Indirect (..)
    , Key
    , csmtCodecs
    )
import Control.Lens (preview)
import Control.Monad.Catch.Pure (Catch, runCatch)
import Control.Monad.Trans.State.Strict
    ( StateT (runStateT)
    , gets
    , modify'
    )
import Data.ByteString (ByteString)
import Data.Foldable (forM_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Database.KV.Transaction
    ( Codecs (..)
    , Column (..)
    , DMap
    , DSum (..)
    , Database (..)
    , Transaction
    , mkCols
    , run
    )

-- | In-memory database type from keys to indirect values
data InMemoryDB = InMemoryDB
    { inMemoryCSMT :: Map ByteString ByteString
    , inMemoryKV :: Map ByteString ByteString
    }
    deriving (Show, Eq)

inMemoryCSMTParsed
    :: StandaloneCodecs k v a
    -> InMemoryDB
    -> Map Key (Indirect a)
inMemoryCSMTParsed StandaloneCodecs{nodeCodec = pa} m =
    let Codecs{keyCodec, valueCodec} = csmtCodecs pa
    in  Map.fromList
            [ (key, aux)
            | (kbs, vbs) <- Map.toList (inMemoryCSMT m)
            , Just key <- [preview keyCodec kbs]
            , Just aux <- [preview valueCodec vbs]
            ]

emptyInMemoryDB :: InMemoryDB
emptyInMemoryDB = InMemoryDB Map.empty Map.empty

onCSMT
    :: (Map ByteString ByteString -> Map ByteString ByteString)
    -> InMemoryDB
    -> InMemoryDB
onCSMT f m = m{inMemoryCSMT = f (inMemoryCSMT m)}

onKV
    :: (Map ByteString ByteString -> Map ByteString ByteString)
    -> InMemoryDB
    -> InMemoryDB
onKV f m = m{inMemoryKV = f (inMemoryKV m)}

-- | Pure monad for CSMT operations
type Pure = StateT InMemoryDB Catch

-- | Run a pure CSMT computation against an in-memory database and throw monadfail
-- failures as errors.
runPure
    :: InMemoryDB
    -> Pure b
    -> (b, InMemoryDB)
runPure p s = case runCatch (runStateT s p) of
    Left err -> error $ "runPure: unexpected error: " ++ show err
    Right res -> res

pureValueAt :: StandaloneCF -> ByteString -> Pure (Maybe ByteString)
pureValueAt StandaloneKV k = do
    kv <- gets inMemoryKV
    pure $ Map.lookup k kv
pureValueAt StandaloneCSMT k = do
    csmt <- gets inMemoryCSMT
    pure $ Map.lookup k csmt

pureApplyOps :: [StandaloneOp] -> Pure ()
pureApplyOps ops = forM_ ops $ \(cf, k, mv) -> case (cf, mv) of
    (StandaloneKV, Nothing) -> modify' $ onKV $ Map.delete k
    (StandaloneKV, Just v) -> modify' $ onKV $ Map.insert k v
    (StandaloneCSMT, Nothing) -> modify' $ onCSMT $ Map.delete k
    (StandaloneCSMT, Just v) -> modify' $ onCSMT $ Map.insert k v

standalonePureCols
    :: StandaloneCodecs k v a
    -> DMap (Standalone k v a) (Column StandaloneCF)
standalonePureCols StandaloneCodecs{keyCodec = pk, valueCodec = pv, nodeCodec = pa} =
    mkCols
        [ StandaloneKVCol
            :=> Column
                { family = StandaloneKV
                , codecs = Codecs pk pv
                }
        , StandaloneCSMTCol
            :=> Column
                { family = StandaloneCSMT
                , codecs = csmtCodecs pa
                }
        ]

pureDatabase
    :: StandaloneCodecs k v a
    -> Database Pure StandaloneCF (Standalone k v a) StandaloneOp
pureDatabase codecs =
    Database
        { valueAt = pureValueAt
        , applyOps = pureApplyOps
        , columns = standalonePureCols codecs
        , mkOperation = mkStandaloneOp
        }

runPureTransaction
    :: StandaloneCodecs k v a
    -> Transaction Pure StandaloneCF (Standalone k v a) StandaloneOp b
    -> Pure b
runPureTransaction codecs = run (pureDatabase codecs)
