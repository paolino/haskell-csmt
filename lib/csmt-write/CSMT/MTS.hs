-- | CSMT implementation of the MTS interface.
--
-- Defines @CsmtImpl@ phantom type with type family instances
-- and constructors that wrap CSMT operations into
-- 'MerkleTreeStore'.
--
-- Full-mode constructors:
--
-- * 'csmtMerkleTreeStoreT' — prefix-scoped transactional store
-- * 'csmtMerkleTreeStore' — IO convenience wrapper
-- * 'csmtNamespacedMTST' — transactional namespaced store
-- * 'csmtNamespacedMTS' — IO namespaced store
--
-- KVOnly-mode constructors:
--
-- * 'csmtKVOnlyStoreT' — transactional, writes KV + journal
-- * 'csmtKVOnlyStore' — IO convenience wrapper
--
-- Journal operations:
--
-- * 'csmtReplayJournal' — replay journal against tree
-- * 'csmtJournalEmpty' — check if journal has entries
--
-- Split-mode operations:
--
-- * 'CommonOps' — shared KV operations for both modes
-- * 'Ops' — GADT indexed by 'Mode' with bidirectional
--   transitions
-- * 'mkKVOnlyOps' — build 'KVOnly' ops with 'toFull' replay
-- * 'mkFullOps' — build 'Full' ops with 'toKVOnly' transition
--
-- Crash recovery:
--
-- * 'DbState' — three-state open result
-- * 'ReadyState' — mode choice after recovery
-- * 'patchSentinelKey' — sentinel key in journal
module CSMT.MTS
    ( CsmtImpl
    , csmtMerkleTreeStoreT
    , csmtMerkleTreeStore
    , csmtNamespacedMTST
    , csmtNamespacedMTS
    , csmtKVOnlyStoreT
    , csmtKVOnlyStore
    , csmtManagedTransition
    , csmtReplayJournal
    , csmtJournalEmpty
    , replayJournalChunkT
    , journalEmptyT

      -- * Split-mode Ops GADT
    , CommonOps (..)
    , Ops
        ( OpsKVOnly
        , OpsFull
        , kvCommon
        , toFull
        , fullCommon
        , opsRootHash
        , toKVOnly
        )
    , mkKVOnlyOps
    , mkFullOps

      -- * Replay tracing
    , ReplayEvent (..)

      -- * Crash recovery
    , DbState (..)
    , ReadyState (..)
    , openOps
    , patchSentinelKey
    , encodePatchSentinel
    , decodePatchSentinel
    , checkPatchRecovery

      -- * Journal helpers (for testing)
    , readJournalChunkT
    , journalEntriesToPatchOps
    )
where

import CSMT.Backend.Standalone
    ( Standalone (..)
    , StandaloneCF
    , StandaloneOp
    )
import CSMT.Deletion
    ( deleteSubtree
    , deleting
    , deletingDirect
    , deletingTreeOnly
    )
import CSMT.Hashes (Hash)
import CSMT.Insertion
    ( allPrefixes
    , bucketIndex
    , expandToBucketDepth
    , inserting
    , insertingDirect
    , insertingTreeOnly
    , mergeSubtreeRoots
    , updatingTreeOnly
    )
import CSMT.Interface
    ( FromKV (..)
    , Hashing (..)
    , Indirect (..)
    , Key
    , getKey
    , putKey
    , root
    )
import CSMT.Populate (PatchOp (..), patchParallel)
import CSMT.Proof.Completeness
    ( CompletenessProof (..)
    , collectValues
    , foldCompletenessProof
    , generateProof
    )
import CSMT.Proof.Insertion
    ( InclusionProof (..)
    , buildInclusionProof
    , computeRootHash
    , verifyInclusionProof
    )
import Control.Concurrent.Async (mapConcurrently_)
import Control.Lens (Iso', review, view)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (foldl')
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Serialize (getWord8, putWord8)
import Data.Serialize.Extra (evalGetM, evalPutM)
import Data.Word (Word8)
import Database.KV.Cursor
    ( Cursor
    , Entry (..)
    , firstEntry
    , nextEntry
    )
import Database.KV.Database (Database, KV)
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , delete
    , insert
    , iterating
    , query
    , runTransactionUnguarded
    )
import MTS.Interface
    ( MerkleTreeStore (..)
    , Mode (..)
    , MtsCompletenessProof
    , MtsHash
    , MtsKV (..)
    , MtsKey
    , MtsLeaf
    , MtsMetrics (..)
    , MtsPrefix
    , MtsProof
    , MtsTransition (..)
    , MtsTree (..)
    , MtsValue
    , NamespacedMTS (..)
    , ReplayEvent (..)
    , hoistMTS
    , hoistNamespacedMTS
    )

-- ------------------------------------------------------------------
-- Metrics helpers
-- ------------------------------------------------------------------

-- | Metric counter key for KV entry count.
kvCountKey :: ByteString
kvCountKey = "kv"

-- | Metric counter key for journal entry count.
journalSizeKey :: ByteString
journalSizeKey = "j"

-- | Read a counter, defaulting to 0 if not set.
readCounter
    :: (Monad m, GCompare d)
    => Selector d ByteString Int
    -> ByteString
    -> Transaction m cf d op Int
readCounter sel key =
    fromMaybe 0 <$> query sel key

-- | Adjust a counter by a delta.
adjustCounter
    :: (Monad m, GCompare d)
    => Selector d ByteString Int
    -> ByteString
    -> Int
    -> Transaction m cf d op ()
adjustCounter sel key delta = do
    current <- readCounter sel key
    insert sel key (current + delta)

-- | Read current MTS metrics from the metrics column.
readMetricsT
    :: (Monad m, GCompare d)
    => Selector d ByteString Int
    -> Transaction m cf d op MtsMetrics
readMetricsT sel =
    MtsMetrics
        <$> readCounter sel kvCountKey
        <*> readCounter sel journalSizeKey

-- | Phantom type tag for the CSMT implementation.
data CsmtImpl

type instance MtsKey CsmtImpl = ByteString
type instance MtsValue CsmtImpl = ByteString
type instance MtsHash CsmtImpl = Hash
type instance MtsProof CsmtImpl = InclusionProof Hash
type instance MtsLeaf CsmtImpl = Indirect Hash
type instance MtsCompletenessProof CsmtImpl = CompletenessProof Hash
type instance MtsPrefix CsmtImpl = Key

-- | Journal entry tag bytes.
journalInsertTag
    , journalUpdateTag
    , journalUpdateOldNewTag
    , journalDeleteTag
        :: ByteString
journalInsertTag = B.singleton 0x01
journalUpdateTag = B.singleton 0x02
journalUpdateOldNewTag = B.singleton 0x03
journalDeleteTag = B.singleton 0x00

-- | Encode a journal insert entry (new key): @0x01 ++ value@.
encodeJournalInsert :: ByteString -> ByteString
encodeJournalInsert v = journalInsertTag <> v

-- | Encode a legacy journal update entry: @0x02 ++ value@.
encodeJournalUpdateLegacy :: ByteString -> ByteString
encodeJournalUpdateLegacy v = journalUpdateTag <> v

-- | Encode a journal update entry with the old CSMT value
-- and the replacement KV value.
encodeJournalUpdate :: ByteString -> ByteString -> ByteString
encodeJournalUpdate old new =
    journalUpdateOldNewTag
        <> encodeLength (B.length old)
        <> old
        <> new

-- | Encode a journal delete entry: @0x00 ++ oldValue@.
encodeJournalDelete :: ByteString -> ByteString
encodeJournalDelete v = journalDeleteTag <> v

data JournalEntry
    = JournalInsert ByteString
    | JournalUpdateLegacy ByteString
    | JournalUpdate ByteString ByteString
    | JournalDelete ByteString

-- | Encode a big-endian 32-bit byte length.
encodeLength :: Int -> ByteString
encodeLength n =
    B.pack
        [ fromIntegral $ n `div` 16777216
        , fromIntegral $ n `div` 65536
        , fromIntegral $ n `div` 256
        , fromIntegral n
        ]

decodeLength :: ByteString -> Int
decodeLength =
    B.foldl' (\acc w -> acc * 256 + fromIntegral w) 0

-- | Parse a journal entry without discarding update payloads.
--
-- Tag @0x03@ stores both the old and new value. Replay needs
-- the old value to remove a value-derived tree prefix before
-- inserting the replacement.
parseJournalEntry :: ByteString -> JournalEntry
parseJournalEntry bs = case B.uncons bs of
    Just (0x01, rest) -> JournalInsert rest
    Just (0x02, rest) -> JournalUpdateLegacy rest
    Just (0x03, rest) ->
        let (lenBytes, payload) = B.splitAt 4 rest
            oldLen = decodeLength lenBytes
            (old, new) = B.splitAt oldLen payload
        in  if B.length lenBytes /= 4 || B.length old /= oldLen
                then error "parseJournalEntry: invalid update payload"
                else JournalUpdate old new
    Just (0x00, rest) -> JournalDelete rest
    _ -> error "parseJournalEntry: invalid tag byte"

-- ------------------------------------------------------------------
-- Crash recovery sentinel
-- ------------------------------------------------------------------

-- | Sentinel tag byte, distinct from journal tags
-- (@0x00@, @0x01@, @0x02@).
patchSentinelTag :: Word8
patchSentinelTag = 0xFF

-- | Sentinel key in the journal column.
--
-- @mempty@ (empty bytestring for 'ByteString') sorts
-- before all real keys in both RocksDB and
-- 'Data.Map.Strict'.
patchSentinelKey :: (Monoid k) => k
patchSentinelKey = mempty

-- | Check if a journal value is a sentinel (tag @0xFF@).
isSentinelValue :: ByteString -> Bool
isSentinelValue bs = case B.uncons bs of
    Just (w, _) -> w == patchSentinelTag
    Nothing -> False

-- | Encode a patch sentinel value.
--
-- Format: @0xFF ++ Word8(bucketBits) ++ encodedPrefix@
encodePatchSentinel :: Int -> Key -> ByteString
encodePatchSentinel bucketBits prefix =
    evalPutM $ do
        putWord8 patchSentinelTag
        putWord8 (fromIntegral bucketBits)
        putKey prefix

-- | Decode a patch sentinel value.
--
-- Returns @Just (bucketBits, prefix)@ if the value starts
-- with @0xFF@, @Nothing@ otherwise.
decodePatchSentinel
    :: ByteString -> Maybe (Int, Key)
decodePatchSentinel = evalGetM $ do
    tag <- getWord8
    if tag /= patchSentinelTag
        then fail "not a sentinel"
        else do
            bits <- fromIntegral <$> getWord8
            pfx <- getKey
            pure (bits, pfx)

-- | Check the journal for a recovery sentinel
-- (transactional).
--
-- Returns @Just (bucketBits, prefix)@ when
-- recovery is needed, @Nothing@ otherwise.
checkPatchRecovery
    :: (Monad m, GCompare d, Ord k, Monoid k)
    => Selector d k ByteString
    -- ^ Journal column
    -> Transaction m cf d op (Maybe (Int, Key))
checkPatchRecovery journalCol = do
    mv <- query journalCol patchSentinelKey
    pure $ mv >>= decodePatchSentinel

-- ------------------------------------------------------------------
-- Three-state open API
-- ------------------------------------------------------------------

-- | Result of opening a database with crash recovery
-- awareness.
--
-- If a sentinel flag is present (from a crashed
-- @toFull@ transition), 'NeedsRecovery' forces the
-- caller to run recovery before accessing any mode.
data DbState m cf d ops k v a
    = -- | Sentinel found. Run recovery first.
      NeedsRecovery
        (IO (DbState m cf d ops k v a))
    | -- | No sentinel. Choose a mode.
      Ready (ReadyState m cf d ops k v a)

-- | Mode choice after recovery (or clean open).
data ReadyState m cf d ops k v a
    = -- | Start in KVOnly mode.
      ChooseKVOnly
        (Ops 'KVOnly m cf d ops k v a)
    | -- | Start in Full mode (replays journal).
      ChooseFull
        (IO (Ops 'Full m cf d ops k v a))

-- | Open the database and check for crash recovery.
--
-- If a sentinel is present from a crashed @toFull@,
-- returns 'NeedsRecovery'. The recovery action runs
-- 'mergeSubtreeRoots' + deletes sentinel, then returns
-- 'Ready'.
--
-- If no sentinel is found, returns 'Ready' immediately.
openOps
    :: (Monad m, GCompare d, Ord k, Monoid k)
    => Key
    -- ^ Prefix
    -> Int
    -- ^ Bucket bits
    -> Int
    -- ^ Chunk size
    -> Selector d k v
    -- ^ KV column
    -> Selector d Key (Indirect a)
    -- ^ CSMT column
    -> Selector d k ByteString
    -- ^ Journal column
    -> Selector d ByteString Int
    -- ^ Metrics column (for journal size counter)
    -> Iso' v ByteString
    -- ^ Journal value serialization
    -> FromKV k v a
    -> Hashing a
    -> (forall b. Transaction m cf d ops b -> IO b)
    -- ^ Transaction runner (guarded, for normal ops)
    -> (forall b. Transaction m cf d ops b -> IO b)
    -- ^ Unguarded runner for parallel replay
    -> (ReplayEvent -> IO ())
    -- ^ Trace callback
    -> IO (DbState m cf d ops k v a)
openOps
    prefix
    bucketBits
    chunkSize
    kvCol
    csmtCol
    journalCol
    metricsCol
    journalIso
    fromKV
    hashing
    runTx
    runTxReplay
    trace = do
        mRecovery <-
            runTx $ checkPatchRecovery journalCol
        case mRecovery of
            Just _ ->
                pure
                    $ NeedsRecovery
                    $ do
                        runTx $ do
                            mergeSubtreeRoots
                                prefix
                                hashing
                                csmtCol
                                bucketBits
                            delete
                                journalCol
                                patchSentinelKey
                        pure $ Ready ready
            Nothing ->
                pure $ Ready ready
      where
        ready =
            ChooseKVOnly
                $ mkKVOnlyOps
                    prefix
                    bucketBits
                    chunkSize
                    kvCol
                    csmtCol
                    journalCol
                    metricsCol
                    journalIso
                    fromKV
                    hashing
                    runTx
                    runTxReplay
                    trace

-- ------------------------------------------------------------------
-- Full mode
-- ------------------------------------------------------------------

-- | Build a transactional 'Full' 'MerkleTreeStore' for CSMT
-- scoped to a prefix.
csmtMerkleTreeStoreT
    :: (Monad m)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> MerkleTreeStore
        'Full
        CsmtImpl
        ( Transaction
            m
            cf
            (Standalone ByteString ByteString Hash)
            op
        )
csmtMerkleTreeStoreT prefix fromKV hashing =
    MkFull kv tree
  where
    kv =
        MtsKV
            { mtsInsert = \k v -> do
                existed <-
                    query StandaloneKVCol k
                inserting
                    prefix
                    fromKV
                    hashing
                    StandaloneKVCol
                    StandaloneCSMTCol
                    k
                    v
                when (isNothing existed)
                    $ adjustCounter
                        StandaloneMetricsCol
                        kvCountKey
                        1
            , mtsDelete = \k -> do
                existed <-
                    query StandaloneKVCol k
                deleting
                    prefix
                    fromKV
                    hashing
                    StandaloneKVCol
                    StandaloneCSMTCol
                    k
                when (isJust existed)
                    $ adjustCounter
                        StandaloneMetricsCol
                        kvCountKey
                        (-1)
            , mtsMetrics =
                readMetricsT StandaloneMetricsCol
            }
    tree =
        MtsTree
            { mtsRootHash =
                root hashing StandaloneCSMTCol prefix
            , mtsMkProof = \k -> do
                mp <-
                    buildInclusionProof
                        prefix
                        fromKV
                        StandaloneKVCol
                        StandaloneCSMTCol
                        k
                case mp of
                    Nothing -> pure Nothing
                    Just (_, proof) -> do
                        mr <-
                            root hashing StandaloneCSMTCol prefix
                        pure $ case mr of
                            Nothing -> Nothing
                            Just r -> Just (r, proof)
            , mtsVerifyProof = \v proof -> do
                mr <- root hashing StandaloneCSMTCol prefix
                pure
                    $ case mr of
                        Nothing -> False
                        Just r ->
                            proofValue proof == fromV fromKV v
                                && verifyInclusionProof
                                    hashing
                                    r
                                    proof
            , mtsFoldProof =
                computeRootHash hashing
            , mtsBatchInsert = \kvs -> do
                newCount <-
                    length . filter id
                        <$> mapM
                            ( \(k, _) ->
                                isNothing
                                    <$> query
                                        StandaloneKVCol
                                        k
                            )
                            kvs
                mapM_
                    ( uncurry
                        ( inserting
                            prefix
                            fromKV
                            hashing
                            StandaloneKVCol
                            StandaloneCSMTCol
                        )
                    )
                    kvs
                when (newCount > 0)
                    $ adjustCounter
                        StandaloneMetricsCol
                        kvCountKey
                        newCount
            , mtsCollectLeaves =
                collectValues StandaloneCSMTCol prefix []
            , mtsMkCompletenessProof =
                generateProof StandaloneCSMTCol prefix []
            , mtsVerifyCompletenessProof = \leaves proof -> do
                currentRoot <-
                    root hashing StandaloneCSMTCol prefix
                pure $ case currentRoot of
                    Just r ->
                        case foldCompletenessProof
                            hashing
                            r
                            []
                            leaves
                            proof of
                            Just computedRoot ->
                                computedRoot == r
                            Nothing -> False
                    Nothing -> False
            }

-- | Build an IO 'Full' 'MerkleTreeStore' for CSMT scoped to a
-- prefix.
--
-- Checks that the journal is empty before constructing the
-- store. Fails if there are unplayed journal entries.
csmtMerkleTreeStore
    :: (MonadFail m)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> (forall b. m b -> IO b)
    -> Database
        m
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> IO (MerkleTreeStore 'Full CsmtImpl IO)
csmtMerkleTreeStore prefix run db fromKV hashing = do
    empty <- csmtJournalEmpty run db
    unless empty
        $ fail
            "csmtMerkleTreeStore: journal is not empty, replay first"
    pure
        $ hoistMTS
            (run . runTransactionUnguarded db)
            (csmtMerkleTreeStoreT prefix fromKV hashing)

-- | Build a transactional 'NamespacedMTS' for CSMT.
csmtNamespacedMTST
    :: (Monad m)
    => FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> NamespacedMTS
        CsmtImpl
        ( Transaction
            m
            cf
            (Standalone ByteString ByteString Hash)
            op
        )
csmtNamespacedMTST fromKV hashing =
    NamespacedMTS
        { nsStore = \prefix ->
            csmtMerkleTreeStoreT prefix fromKV hashing
        , nsDelete =
            deleteSubtree StandaloneCSMTCol
        }

-- | Build an IO 'NamespacedMTS' for CSMT.
csmtNamespacedMTS
    :: (MonadFail m)
    => (forall b. m b -> IO b)
    -> Database
        m
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> NamespacedMTS CsmtImpl IO
csmtNamespacedMTS run db fromKV hashing =
    hoistNamespacedMTS
        (run . runTransactionUnguarded db)
        (csmtNamespacedMTST fromKV hashing)

-- ------------------------------------------------------------------
-- KVOnly mode
-- ------------------------------------------------------------------

-- | Build a transactional 'KVOnly' 'MerkleTreeStore' for CSMT.
--
-- Each insert/delete writes KV + journal atomically.
-- No tree operations are available.
csmtKVOnlyStoreT
    :: (Monad m)
    => FromKV ByteString ByteString Hash
    -> MerkleTreeStore
        'KVOnly
        CsmtImpl
        ( Transaction
            m
            cf
            (Standalone ByteString ByteString Hash)
            op
        )
csmtKVOnlyStoreT _fromKV =
    MkKVOnly
        MtsKV
            { mtsInsert = \k v -> do
                mj <- query StandaloneJournalCol k
                existing <- query StandaloneKVCol k
                let journalValue =
                        case parseJournalEntry <$> mj of
                            Just (JournalInsert _) ->
                                encodeJournalInsert v
                            Just (JournalUpdate old _) ->
                                encodeJournalUpdate old v
                            Just (JournalUpdateLegacy _) ->
                                encodeJournalUpdateLegacy v
                            Just (JournalDelete old) ->
                                encodeJournalUpdate old v
                            Nothing ->
                                case existing of
                                    Nothing -> encodeJournalInsert v
                                    Just old -> encodeJournalUpdate old v
                insert StandaloneKVCol k v
                insert StandaloneJournalCol k journalValue
                -- Metrics: new KV key → kvCount +1
                when (isNothing existing)
                    $ adjustCounter
                        StandaloneMetricsCol
                        kvCountKey
                        1
                -- Metrics: new journal entry → journalSize +1
                when (isNothing mj)
                    $ adjustCounter
                        StandaloneMetricsCol
                        journalSizeKey
                        1
            , mtsDelete = \k -> do
                mv <- query StandaloneKVCol k
                case mv of
                    Nothing -> pure ()
                    Just v -> do
                        delete StandaloneKVCol k
                        adjustCounter
                            StandaloneMetricsCol
                            kvCountKey
                            (-1)
                        mj <- query StandaloneJournalCol k
                        case parseJournalEntry <$> mj of
                            Just (JournalInsert _) -> do
                                delete StandaloneJournalCol k
                                adjustCounter
                                    StandaloneMetricsCol
                                    journalSizeKey
                                    (-1)
                            Nothing -> do
                                insert
                                    StandaloneJournalCol
                                    k
                                    (encodeJournalDelete v)
                                adjustCounter
                                    StandaloneMetricsCol
                                    journalSizeKey
                                    1
                            Just (JournalUpdate old _) ->
                                insert
                                    StandaloneJournalCol
                                    k
                                    (encodeJournalDelete old)
                            Just (JournalUpdateLegacy _) ->
                                insert
                                    StandaloneJournalCol
                                    k
                                    (encodeJournalDelete v)
                            Just (JournalDelete old) ->
                                insert
                                    StandaloneJournalCol
                                    k
                                    (encodeJournalDelete old)
            , mtsMetrics =
                readMetricsT StandaloneMetricsCol
            }

-- | Build an IO 'KVOnly' 'MerkleTreeStore' for CSMT.
csmtKVOnlyStore
    :: (MonadFail m)
    => (forall b. m b -> IO b)
    -> Database
        m
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
    -> FromKV ByteString ByteString Hash
    -> MerkleTreeStore 'KVOnly CsmtImpl IO
csmtKVOnlyStore run db fromKV =
    hoistMTS
        (run . runTransactionUnguarded db)
        (csmtKVOnlyStoreT fromKV)

-- ------------------------------------------------------------------
-- Managed transition
-- ------------------------------------------------------------------

-- | Create a managed lifecycle handle for CSMT.
--
-- Returns a 'MtsTransition' that bundles a 'KVOnly' store with
-- a one-shot transition action. After 'transitionToFull' is
-- called, any operation on 'transitionKVStore' throws.
csmtManagedTransition
    :: forall m
     . (MonadFail m)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> Int
    -- ^ Chunk size for journal replay
    -> (forall b. m b -> IO b)
    -> Database
        m
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> IO (MtsTransition CsmtImpl IO)
csmtManagedTransition prefix chunkSize run db fromKV hashing = do
    locked <- newIORef False
    let guardedRun
            :: forall b
             . Transaction
                m
                StandaloneCF
                (Standalone ByteString ByteString Hash)
                StandaloneOp
                b
            -> IO b
        guardedRun txn = do
            isLocked <- readIORef locked
            when isLocked
                $ fail
                    "KVOnly store disabled after transition"
            run (runTransactionUnguarded db txn)
    pure
        MtsTransition
            { transitionKVStore =
                hoistMTS
                    guardedRun
                    (csmtKVOnlyStoreT fromKV)
            , transitionToFull = do
                writeIORef locked True
                csmtReplayJournal
                    prefix
                    chunkSize
                    run
                    db
                    fromKV
                    hashing
                    (const $ pure ())
                pure
                    $ hoistMTS
                        (run . runTransactionUnguarded db)
                        ( csmtMerkleTreeStoreT
                            prefix
                            fromKV
                            hashing
                        )
            }

-- ------------------------------------------------------------------
-- Journal replay
-- ------------------------------------------------------------------

-- | Check if the journal column is empty.
csmtJournalEmpty
    :: (MonadFail m)
    => (forall b. m b -> IO b)
    -> Database
        m
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
    -> IO Bool
csmtJournalEmpty run db =
    run
        $ runTransactionUnguarded db
        $ journalEmptyT StandaloneJournalCol

-- | Replay journal entries against the tree, then clear them.
--
-- Processes entries in chunks. Each chunk reads up to
-- @chunkSize@ entries, applies tree-only operations, and
-- deletes the replayed journal entries, all in one transaction.
-- Repeats until the journal is empty. The trace callback
-- receives 'ReplayStart' and 'ReplayStop' events per chunk.
csmtReplayJournal
    :: (MonadFail m)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> Int
    -- ^ Chunk size
    -> (forall b. m b -> IO b)
    -> Database
        m
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> (ReplayEvent -> IO ())
    -- ^ Trace callback (called per replay chunk)
    -> IO ()
csmtReplayJournal
    prefix
    chunkSize
    run
    db
    fromKV
    hashing
    trace = do
        journalSize <-
            run
                $ runTransactionUnguarded db
                $ readCounter
                    StandaloneMetricsCol
                    journalSizeKey
        loop journalSize
      where
        loop remaining = do
            result <-
                run
                    $ runTransactionUnguarded db
                    $ replayJournalChunkT
                        prefix
                        chunkSize
                        fromKV
                        hashing
            case result of
                Nothing -> pure ()
                Just n -> do
                    let remaining' = remaining - n
                    trace
                        ReplayStart
                            { rsChunkSize = n
                            , rsBuckets = 1
                            , rsTotalBuckets = 1
                            , rsOpsPerBucket = [n]
                            , rsEntriesRemaining =
                                remaining'
                            }
                    trace ReplayStop
                    loop remaining'

-- | Collect up to @n@ more entries after the first.
collectN
    :: (Monad m)
    => Int
    -> [Entry c]
    -> Cursor m c [Entry c]
collectN 0 acc = pure (reverse acc)
collectN n acc = do
    me <- nextEntry
    case me of
        Nothing -> pure (reverse acc)
        Just e -> collectN (n - 1) (e : acc)

-- | Check if the journal column is empty (transactional).
--
-- Polymorphic in @cf@, @op@, and column type @d@ so it can
-- be used with any column definition.
journalEmptyT
    :: (Monad m, GCompare d)
    => Selector d k ByteString
    -- ^ Journal column
    -> Transaction m cf d op Bool
journalEmptyT journalCol = do
    me <- iterating journalCol $ do
        e <- firstEntry
        case e of
            Nothing -> pure Nothing
            Just entry
                | isSentinelValue (entryValue entry) ->
                    nextEntry
                | otherwise -> pure (Just entry)
    pure $ case me of
        Nothing -> True
        Just _ -> False

-- | Process one chunk of journal entries (transactional).
--
-- Reads up to @chunkSize@ journal entries, applies tree-only
-- operations, and deletes the replayed entries. Returns
-- @Nothing@ when the journal is empty (all done), or
-- @Just n@ with the number of entries processed.
--
-- Polymorphic in @cf@ and @op@ so it can be composed with
-- 'mapColumns' into richer column types.
replayJournalChunkT
    :: (Monad m)
    => Key
    -- ^ Prefix (use @[]@ for root)
    -> Int
    -- ^ Chunk size
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> Transaction
        m
        cf
        (Standalone ByteString ByteString Hash)
        op
        (Maybe Int)
replayJournalChunkT prefix chunkSize fromKV hashing = do
    entries <- iterating StandaloneJournalCol $ do
        me <- firstEntry
        case me of
            Nothing -> pure []
            Just e
                | isSentinelValue (entryValue e) ->
                    collectN chunkSize []
                | otherwise ->
                    collectN (chunkSize - 1) [e]
    if null entries
        then pure Nothing
        else do
            let n = length entries
            replayEntries prefix fromKV hashing entries
            adjustCounter
                StandaloneMetricsCol
                journalSizeKey
                (negate n)
            pure $ Just n

-- | Apply journal entries to the tree and clear them.
replayEntries
    :: (Monad m)
    => Key
    -> FromKV ByteString ByteString Hash
    -> Hashing Hash
    -> [Entry (KV ByteString ByteString)]
    -> Transaction
        m
        cf
        (Standalone ByteString ByteString Hash)
        op
        ()
replayEntries prefix fromKV hashing entries = do
    mapM_ applyEntry entries
    mapM_
        (delete StandaloneJournalCol . entryKey)
        entries
  where
    applyEntry e =
        let k = entryKey e
        in  case parseJournalEntry (entryValue e) of
                JournalInsert v ->
                    insertingTreeOnly
                        prefix
                        fromKV
                        hashing
                        StandaloneCSMTCol
                        k
                        v
                JournalUpdateLegacy _ ->
                    error
                        "replayEntries: legacy journal update \
                        \cannot relocate value-derived tree prefix"
                JournalUpdate old new -> do
                    updatingTreeOnly
                        prefix
                        fromKV
                        hashing
                        StandaloneCSMTCol
                        k
                        old
                        new
                JournalDelete v ->
                    deletingTreeOnly
                        prefix
                        fromKV
                        hashing
                        StandaloneCSMTCol
                        k
                        v

-- ------------------------------------------------------------------
-- Split-mode Ops GADT
-- ------------------------------------------------------------------

-- | Shared KV operations available in both modes.
data CommonOps m cf d ops k v = CommonOps
    { opsInsert
        :: k
        -> v
        -> Transaction m cf d ops ()
    -- ^ Insert a key-value pair
    , opsDelete
        :: k
        -> Transaction m cf d ops ()
    -- ^ Delete a key
    , opsQuery
        :: k
        -> Transaction m cf d ops (Maybe v)
    -- ^ Query a key
    }

-- | Mode-indexed operations with bidirectional transitions.
--
-- In 'KVOnly' mode, mutations write KV + journal. 'toFull'
-- replays the journal via 'patchParallel' and returns 'Full'
-- ops.
--
-- In 'Full' mode, mutations write KV + update CSMT tree.
-- 'toKVOnly' verifies the journal is empty and returns
-- 'KVOnly' ops. Fails if journal is not empty.
data Ops (mode :: Mode) m cf d ops k v a where
    OpsKVOnly
        :: { kvCommon :: CommonOps m cf d ops k v
           , toFull
                :: IO (Maybe (Ops 'Full m cf d ops k v a))
           }
        -> Ops 'KVOnly m cf d ops k v a
    OpsFull
        :: { fullCommon :: CommonOps m cf d ops k v
           , opsRootHash
                :: Transaction m cf d ops (Maybe a)
           , toKVOnly
                :: IO
                    ( Maybe
                        ( Ops
                            'KVOnly
                            m
                            cf
                            d
                            ops
                            k
                            v
                            a
                        )
                    )
           }
        -> Ops 'Full m cf d ops k v a

-- | Build 'KVOnly' ops for generic column types.
--
-- Insert/delete write KV + journal atomically. Query reads
-- KV. 'toFull' replays the journal via 'patchParallel'.
mkKVOnlyOps
    :: (Monad m, GCompare d, Ord k, Monoid k)
    => Key
    -- ^ Prefix
    -> Int
    -- ^ Bucket bits for parallel replay
    -> Int
    -- ^ Chunk size for journal batches
    -> Selector d k v
    -- ^ KV column
    -> Selector d Key (Indirect a)
    -- ^ CSMT column
    -> Selector d k ByteString
    -- ^ Journal column
    -> Selector d ByteString Int
    -- ^ Metrics column (for journal size counter)
    -> Iso' v ByteString
    -- ^ Journal value serialization
    -> FromKV k v a
    -> Hashing a
    -> (forall b. Transaction m cf d ops b -> IO b)
    -- ^ Guarded runner for normal ops
    -> (forall b. Transaction m cf d ops b -> IO b)
    -- ^ Unguarded runner for parallel replay
    -> (ReplayEvent -> IO ())
    -- ^ Trace callback (called per replay chunk)
    -> Ops 'KVOnly m cf d ops k v a
mkKVOnlyOps
    prefix
    bucketBits
    chunkSize
    kvCol
    csmtCol
    journalCol
    metricsCol
    journalIso
    fromKV
    hashing
    runTx
    runTxReplay
    trace =
        OpsKVOnly
            { kvCommon =
                CommonOps
                    { -- Journal tag compositions:
                      --
                      -- INSERT (journal × KV → journal'):
                      --   Nothing × Nothing → JInsert  (new key)
                      --   Nothing × Just old
                      --     → JUpdate old new (key from CSMT)
                      --   JInsert × _       → JInsert  (still new)
                      --   JUpdate old _ × _
                      --     → JUpdate old new (still CSMT)
                      --   JDelete old × _
                      --     → JUpdate old new (re-insert, CSMT has it)
                      --
                      -- DELETE (journal → journal'):
                      --   Nothing           → JDelete  (key from CSMT)
                      --   JInsert           → ∅ elide   (new, not in CSMT)
                      --   JUpdate old _     → JDelete old
                      --   JDelete           → ⊥         (KV empty, unreachable)
                      opsInsert = \k v -> do
                        mj <- query journalCol k
                        existing <- query kvCol k
                        let encoded = view journalIso v
                            journalValue =
                                case parseJournalEntry <$> mj of
                                    Just (JournalInsert _) ->
                                        encodeJournalInsert encoded
                                    Just (JournalUpdate old _) ->
                                        encodeJournalUpdate old encoded
                                    Just (JournalUpdateLegacy _) ->
                                        encodeJournalUpdateLegacy encoded
                                    Just (JournalDelete old) ->
                                        encodeJournalUpdate old encoded
                                    Nothing ->
                                        case existing of
                                            Nothing ->
                                                encodeJournalInsert
                                                    encoded
                                            Just old ->
                                                encodeJournalUpdate
                                                    (view journalIso old)
                                                    encoded
                        insert kvCol k v
                        insert journalCol k journalValue
                        -- New journal entry → journalSize +1
                        when (isNothing mj)
                            $ adjustCounter
                                metricsCol
                                journalSizeKey
                                1
                    , opsDelete = \k -> do
                        mv <- query kvCol k
                        case mv of
                            Nothing -> pure ()
                            Just v -> do
                                delete kvCol k
                                mj <- query journalCol k
                                case parseJournalEntry <$> mj of
                                    Just (JournalInsert _) -> do
                                        delete journalCol k
                                        adjustCounter
                                            metricsCol
                                            journalSizeKey
                                            (-1)
                                    Nothing -> do
                                        insert
                                            journalCol
                                            k
                                            ( encodeJournalDelete
                                                (view journalIso v)
                                            )
                                        adjustCounter
                                            metricsCol
                                            journalSizeKey
                                            1
                                    Just (JournalUpdate old _) ->
                                        insert
                                            journalCol
                                            k
                                            (encodeJournalDelete old)
                                    Just (JournalUpdateLegacy _) ->
                                        insert
                                            journalCol
                                            k
                                            ( encodeJournalDelete
                                                (view journalIso v)
                                            )
                                    Just (JournalDelete old) ->
                                        insert
                                            journalCol
                                            k
                                            (encodeJournalDelete old)
                    , opsQuery = query kvCol
                    }
            , toFull = do
                -- Write sentinel + expand atomically
                journalSize <- runTx $ do
                    insert
                        journalCol
                        patchSentinelKey
                        ( encodePatchSentinel
                            bucketBits
                            prefix
                        )
                    expandToBucketDepth
                        prefix
                        bucketBits
                        csmtCol
                    readCounter metricsCol journalSizeKey
                replayLoop journalSize
                -- Reset journal counter (entries were
                -- deleted by patchParallel without
                -- decrementing)
                runTx
                    $ insert metricsCol journalSizeKey 0
                -- Merge + delete sentinel atomically
                runTx $ do
                    mergeSubtreeRoots
                        prefix
                        hashing
                        csmtCol
                        bucketBits
                    delete
                        journalCol
                        patchSentinelKey
                pure
                    $ Just
                    $ mkFullOps
                        prefix
                        bucketBits
                        chunkSize
                        kvCol
                        csmtCol
                        journalCol
                        metricsCol
                        journalIso
                        fromKV
                        hashing
                        runTx
                        runTxReplay
                        trace
            }
      where
        totalBuckets = 2 ^ bucketBits :: Int
        replayLoop remaining = do
            entries <-
                runTxReplay
                    $ readJournalChunkT
                        journalCol
                        chunkSize
            if null entries
                then pure ()
                else do
                    let n = length entries
                        remaining' = remaining - n
                        (normalOps, updateOps, updateFinalizers) =
                            journalEntriesToReplayWork
                                journalIso
                                fromKV
                                hashing
                                csmtCol
                                journalCol
                                prefix
                                entries
                        normalBucketTxns =
                            patchParallel
                                bucketBits
                                prefix
                                hashing
                                csmtCol
                                journalCol
                                normalOps
                        updateBucketTxns =
                            patchParallelTreeOnly
                                bucketBits
                                prefix
                                hashing
                                csmtCol
                                updateOps
                        bucketTxns =
                            normalBucketTxns
                                <> updateBucketTxns
                    trace
                        ReplayStart
                            { rsChunkSize = n
                            , rsBuckets = length bucketTxns
                            , rsTotalBuckets = totalBuckets
                            , rsOpsPerBucket =
                                map fst bucketTxns
                            , rsEntriesRemaining =
                                remaining'
                            }
                    mapConcurrently_
                        (runTxReplay . snd)
                        bucketTxns
                    mapM_ runTxReplay updateFinalizers
                    trace ReplayStop
                    replayLoop remaining'

-- | Build 'Full' ops for generic column types.
--
-- Insert/delete write KV + update CSMT tree. Query reads KV.
-- 'toKVOnly' verifies journal is empty and returns 'KVOnly'
-- ops.
mkFullOps
    :: (Monad m, GCompare d, Ord k, Monoid k)
    => Key
    -- ^ Prefix
    -> Int
    -- ^ Bucket bits (passed through to 'mkKVOnlyOps')
    -> Int
    -- ^ Chunk size (passed through to 'mkKVOnlyOps')
    -> Selector d k v
    -- ^ KV column
    -> Selector d Key (Indirect a)
    -- ^ CSMT column
    -> Selector d k ByteString
    -- ^ Journal column
    -> Selector d ByteString Int
    -- ^ Metrics column (passed through to 'mkKVOnlyOps')
    -> Iso' v ByteString
    -- ^ Journal value serialization
    -> FromKV k v a
    -> Hashing a
    -> (forall b. Transaction m cf d ops b -> IO b)
    -- ^ Guarded runner for normal ops
    -> (forall b. Transaction m cf d ops b -> IO b)
    -- ^ Unguarded runner for parallel replay
    -> (ReplayEvent -> IO ())
    -- ^ Trace callback (passed through to 'mkKVOnlyOps')
    -> Ops 'Full m cf d ops k v a
mkFullOps
    prefix
    bucketBits
    chunkSize
    kvCol
    csmtCol
    journalCol
    metricsCol
    journalIso
    fromKV
    hashing
    runTx
    runTxReplay
    trace =
        OpsFull
            { fullCommon =
                CommonOps
                    { opsInsert =
                        inserting
                            prefix
                            fromKV
                            hashing
                            kvCol
                            csmtCol
                    , opsDelete =
                        deleting
                            prefix
                            fromKV
                            hashing
                            kvCol
                            csmtCol
                    , opsQuery = query kvCol
                    }
            , opsRootHash =
                root hashing csmtCol prefix
            , toKVOnly = do
                empty <- runTx (journalEmptyT journalCol)
                if empty
                    then
                        pure
                            $ Just
                            $ mkKVOnlyOps
                                prefix
                                bucketBits
                                chunkSize
                                kvCol
                                csmtCol
                                journalCol
                                metricsCol
                                journalIso
                                fromKV
                                hashing
                                runTx
                                runTxReplay
                                trace
                    else pure Nothing
            }

-- | Read up to @n@ journal entries (transactional).
--
-- Skips sentinel entries (tag byte @0xFF@) that may
-- be present during a 'toFull' transition.
readJournalChunkT
    :: (Monad m, GCompare d)
    => Selector d k ByteString
    -- ^ Journal column
    -> Int
    -> Transaction m cf d op [(k, ByteString)]
readJournalChunkT journalCol chunkSize = do
    entries <- iterating journalCol $ do
        me <- firstEntry
        case me of
            Nothing -> pure []
            Just e
                | isSentinelValue (entryValue e) ->
                    -- Sentinel is first: skip it and
                    -- collect chunkSize from remainder
                    collectN chunkSize []
                | otherwise ->
                    collectN (chunkSize - 1) [e]
    pure [(entryKey e, entryValue e) | e <- entries]

-- | Convert journal entries into replay work.
--
-- Inserts and deletes are replayed through bucketed
-- 'patchParallel', which also deletes their journal entries.
-- Updates become two tree-only bucket operations: delete the old
-- value-derived tree key and insert the new one. Their journal entry
-- is deleted only after both bucket operations have completed, so a
-- crash can safely replay the idempotent update again.
journalEntriesToReplayWork
    :: (GCompare d, Ord k)
    => Iso' v ByteString
    -- ^ Journal value serialization
    -> FromKV k v a
    -> Hashing a
    -> Selector d Key (Indirect a)
    -- ^ CSMT column
    -> Selector d k ByteString
    -- ^ Journal column
    -> Key
    -- ^ Prefix
    -> [(k, ByteString)]
    -- ^ (journal key, encoded journal value)
    -> ( [(k, PatchOp Key a)]
       , [PatchOp Key a]
       , [Transaction m cf d op ()]
       )
journalEntriesToReplayWork
    journalIso
    fromKV
    _hashing
    _csmtCol
    journalCol
    _prefix =
        foldr convert ([], [], [])
      where
        treeKey =
            treePrefix fromKV

        convert (k, raw) (normalOps, updateOps, finalizers) =
            case parseJournalEntry raw of
                JournalInsert serializedV ->
                    ( patchInsert k serializedV : normalOps
                    , updateOps
                    , finalizers
                    )
                JournalUpdateLegacy _ ->
                    error
                        "journalEntriesToReplayWork: legacy journal \
                        \update cannot relocate value-derived tree \
                        \prefix"
                JournalUpdate serializedOld serializedNew ->
                    let old = review journalIso serializedOld
                        new = review journalIso serializedNew
                        oldTreeKey =
                            treeKey old <> view (isoK fromKV) k
                        newTreeKey =
                            treeKey new <> view (isoK fromKV) k
                        updateOps' =
                            if oldTreeKey == newTreeKey
                                then
                                    PatchInsert
                                        newTreeKey
                                        (fromV fromKV new)
                                        : updateOps
                                else
                                    PatchDelete oldTreeKey
                                        : PatchInsert
                                            newTreeKey
                                            (fromV fromKV new)
                                        : updateOps
                    in  ( normalOps
                        , updateOps'
                        , delete journalCol k : finalizers
                        )
                JournalDelete serializedV ->
                    ( patchDelete k serializedV : normalOps
                    , updateOps
                    , finalizers
                    )

        patchInsert k serializedV =
            let v = review journalIso serializedV
            in  ( k
                , PatchInsert
                    (treeKey v <> view (isoK fromKV) k)
                    (fromV fromKV v)
                )

        patchDelete k serializedV =
            let v = review journalIso serializedV
            in  ( k
                , PatchDelete
                    (treeKey v <> view (isoK fromKV) k)
                )

patchParallelTreeOnly
    :: (GCompare d, Monad m)
    => Int
    -> Key
    -> Hashing a
    -> Selector d Key (Indirect a)
    -> [PatchOp Key a]
    -> [(Int, Transaction m cf d ops ())]
patchParallelTreeOnly bucketBits pfx hashing csmtCol entries =
    map mkBucketTx (Map.toList buckets)
  where
    prefixes = allPrefixes bucketBits

    buckets =
        foldl' addEntry Map.empty entries

    addEntry m op =
        let treeKey = opKey op
            (bucket, stripped) = splitAt bucketBits treeKey
            idx = bucketIndex bucket
            op' = setOpKey stripped op
        in  Map.insertWith (++) idx [op'] m

    mkBucketTx (idx, ops) =
        let bpfx = pfx <> (prefixes !! idx)
        in  ( length ops
            , mapM_ (applyOp bpfx) ops
            )

    applyOp bpfx (PatchInsert k v) =
        insertingDirect bpfx hashing csmtCol k v
    applyOp bpfx (PatchDelete k) =
        deletingDirect bpfx hashing csmtCol k

    opKey (PatchInsert k _) = k
    opKey (PatchDelete k) = k

    setOpKey k (PatchInsert _ v) = PatchInsert k v
    setOpKey k (PatchDelete _) = PatchDelete k

-- | Convert journal entries to 'PatchOp' pairs for
-- 'patchParallel'.
journalEntriesToPatchOps
    :: Iso' v ByteString
    -- ^ Journal value serialization
    -> FromKV k v a
    -> [(k, ByteString)]
    -- ^ (journal key, encoded journal value)
    -> [(k, PatchOp Key a)]
journalEntriesToPatchOps journalIso fromKV = map convert
  where
    convert (k, raw) =
        case parseJournalEntry raw of
            JournalInsert serializedV ->
                patchInsert k serializedV
            JournalUpdateLegacy _ ->
                error
                    "journalEntriesToPatchOps: legacy journal update \
                    \cannot relocate value-derived tree prefix"
            JournalUpdate _ _ ->
                error
                    "journalEntriesToPatchOps: journal update needs \
                    \update-aware replay"
            JournalDelete serializedV ->
                patchDelete k serializedV

    patchInsert k serializedV =
        let v = review journalIso serializedV
            treeKey =
                treePrefix fromKV v <> view (isoK fromKV) k
            hash = fromV fromKV v
        in  (k, PatchInsert treeKey hash)

    patchDelete k serializedV =
        let v = review journalIso serializedV
            treeKey =
                treePrefix fromKV v <> view (isoK fromKV) k
        in  (k, PatchDelete treeKey)
