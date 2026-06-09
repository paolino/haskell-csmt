{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module CSMT.PrefixReconciliationSpec (spec) where

import CSMT.Backend.Pure
    ( InMemoryDB
    , Pure
    , emptyInMemoryDB
    , pureDatabase
    , runPure
    , runPureTransaction
    )
import CSMT.Backend.Standalone
    ( Standalone (..)
    , StandaloneCF
    , StandaloneCodecs (..)
    , StandaloneOp
    )
import CSMT.Deletion qualified
import CSMT.Hashes
    ( Hash
    , fromKVHashes
    , hashHashing
    , isoHash
    )
import CSMT.Insertion qualified
import CSMT.Interface
    ( Direction (L, R)
    , FromKV (..)
    , Indirect (..)
    , root
    )
import CSMT.MTS
    ( CommonOps (..)
    , DbState (..)
    , Ops (..)
    , ReadyState (..)
    , mkKVOnlyOps
    , openOps
    )
import CSMT.Proof.Completeness (collectValues)
import Control.Lens (iso, view)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.IORef
    ( newIORef
    , readIORef
    , writeIORef
    )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Database.KV.Cursor
    ( Cursor
    , Entry (..)
    , firstEntry
    , nextEntry
    )
import Database.KV.Database (KeyOf, ValueOf)
import Database.KV.Transaction
    ( Transaction
    , iterating
    , runTransactionUnguarded
    )
import MTS.Interface (Mode (..))
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Gen
    , arbitrary
    , chooseInt
    , elements
    , forAllShrink
    , frequency
    , property
    , shrinkList
    , vectorOf
    )

data KVOp
    = Put ByteString ByteString
    | Del ByteString
    deriving stock (Eq, Show)

data Relocation = Relocation ByteString ByteString ByteString
    deriving stock (Eq, Show)

newtype EquivalentModel = EquivalentModel [(ByteString, ByteString)]
    deriving stock (Eq, Show)

type LeafSet = Set ([Direction], Hash)

csmtCodecs :: StandaloneCodecs ByteString ByteString Hash
csmtCodecs =
    StandaloneCodecs
        { keyCodec = iso id id
        , valueCodec = iso id id
        , nodeCodec = isoHash
        }

prefixedFromKV :: FromKV ByteString ByteString Hash
prefixedFromKV =
    fromKVHashes
        { treePrefix = \v ->
            case B.uncons v of
                Just (w, _) | w < 128 -> [L]
                _ -> [R]
        }

genKey :: Gen ByteString
genKey =
    elements
        [ "103d753d77c8c541"
        , "wallet-input-0"
        , "wallet-input-1"
        , "wallet-input-2"
        , "request-input"
        , "state-input"
        ]

genPayload :: Gen ByteString
genPayload = B.pack <$> vectorOf 7 arbitrary

genValueWithPrefix :: Word -> Gen ByteString
genValueWithPrefix marker =
    B.cons (fromIntegral marker) <$> genPayload

genValue :: Gen ByteString
genValue =
    frequency
        [ (1, genValueWithPrefix 0)
        , (1, genValueWithPrefix 255)
        , (2, B.cons <$> arbitrary <*> genPayload)
        ]

genOps :: Gen [KVOp]
genOps = do
    n <- chooseInt (1, 40)
    vectorOf n genOp
  where
    genOp =
        frequency
            [ (7, Put <$> genKey <*> genValue)
            , (3, Del <$> genKey)
            ]

shrinkOps :: [KVOp] -> [[KVOp]]
shrinkOps =
    shrinkList shrinkOp
  where
    shrinkOp (Put k v) =
        [Put k' v | k' <- shrinkBS k]
            <> [Put k v' | v' <- shrinkValue v]
            <> [Del k]
    shrinkOp (Del k) =
        [Del k' | k' <- shrinkBS k]

shrinkBS :: ByteString -> [ByteString]
shrinkBS bs =
    [ B.take n bs
    | n <- [1 .. B.length bs - 1]
    ]

shrinkValue :: ByteString -> [ByteString]
shrinkValue v =
    [ v'
    | v' <- shrinkBS v
    , not (B.null v')
    ]

genRelocation :: Gen Relocation
genRelocation =
    Relocation
        <$> genKey
        <*> genValueWithPrefix 0
        <*> genValueWithPrefix 255

shrinkRelocation :: Relocation -> [Relocation]
shrinkRelocation (Relocation k old new) =
    [Relocation k' old new | k' <- shrinkBS k]
        <> [Relocation k old' new | old' <- shrinkValue old, oldPrefix old']
        <> [Relocation k old new' | new' <- shrinkValue new, newPrefix new']
  where
    oldPrefix v = treePrefix prefixedFromKV v == [L]
    newPrefix v = treePrefix prefixedFromKV v == [R]

genEquivalentModel :: Gen EquivalentModel
genEquivalentModel = do
    n <- chooseInt (1, 12)
    pairs <- vectorOf n ((,) <$> genKey <*> genValue)
    pure $ EquivalentModel $ Map.toList $ Map.fromList pairs

shrinkEquivalentModel :: EquivalentModel -> [EquivalentModel]
shrinkEquivalentModel (EquivalentModel pairs) =
    [ EquivalentModel $ Map.toList $ Map.fromList pairs'
    | pairs' <- shrinkList shrinkPair pairs
    , not (null pairs')
    ]
  where
    shrinkPair (k, v) =
        [(k', v) | k' <- shrinkBS k]
            <> [(k, v') | v' <- shrinkValue v]

modelOps :: [KVOp] -> Map ByteString ByteString
modelOps =
    foldl apply Map.empty
  where
    apply m = \case
        Put k v -> Map.insert k v m
        Del k -> Map.delete k m

expectedLeaves :: Map ByteString ByteString -> LeafSet
expectedLeaves =
    Set.fromList
        . map expectedLeaf
        . Map.toList
  where
    expectedLeaf (k, v) =
        ( treePrefix prefixedFromKV v
            <> view (isoK prefixedFromKV) k
        , fromV prefixedFromKV v
        )

collectAll :: (Monad m) => Cursor m c [(KeyOf c, ValueOf c)]
collectAll = do
    me <- firstEntry
    case me of
        Nothing -> pure []
        Just e -> go [(entryKey e, entryValue e)]
  where
    go acc = do
        me <- nextEntry
        case me of
            Nothing -> pure $ reverse acc
            Just e -> go ((entryKey e, entryValue e) : acc)

leafSetT
    :: Transaction
        Pure
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
        LeafSet
leafSetT =
    Set.fromList
        . map (\Indirect{jump, value} -> (jump, value))
        <$> collectValues StandaloneCSMTCol [] []

kvMapT
    :: Transaction
        Pure
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
        (Map ByteString ByteString)
kvMapT =
    Map.fromList <$> iterating StandaloneKVCol collectAll

rootT
    :: Transaction
        Pure
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
        (Maybe Hash)
rootT = root hashHashing StandaloneCSMTCol []

runFull :: [KVOp] -> InMemoryDB
runFull ops =
    snd
        $ runPure emptyInMemoryDB
        $ mapM_ apply ops
  where
    apply = \case
        Put k v ->
            runPureTransaction csmtCodecs
                $ CSMT.Insertion.inserting
                    []
                    prefixedFromKV
                    hashHashing
                    StandaloneKVCol
                    StandaloneCSMTCol
                    k
                    v
        Del k ->
            runPureTransaction csmtCodecs
                $ CSMT.Deletion.deleting
                    []
                    prefixedFromKV
                    hashHashing
                    StandaloneKVCol
                    StandaloneCSMTCol
                    k

data KVOnlyResult = KVOnlyResult
    { kvOnlyRoot :: Maybe Hash
    , kvOnlyLeaves :: LeafSet
    , kvOnlyKV :: Map ByteString ByteString
    }

withKVOnly
    :: [KVOp]
    -> ( Ops
            'KVOnly
            Pure
            StandaloneCF
            (Standalone ByteString ByteString Hash)
            StandaloneOp
            ByteString
            ByteString
            Hash
         -> ( forall b
               . Transaction
                    Pure
                    StandaloneCF
                    (Standalone ByteString ByteString Hash)
                    StandaloneOp
                    b
              -> IO b
            )
         -> IO a
       )
    -> IO a
withKVOnly ops action = do
    ref <- newIORef emptyInMemoryDB
    let run :: forall b. Pure b -> IO b
        run act = do
            db <- readIORef ref
            let (a, db') = runPure db act
            writeIORef ref db'
            pure a
        rtx = run . runTransactionUnguarded (pureDatabase csmtCodecs)
        kvOps =
            mkKVOnlyOps
                []
                2
                100
                StandaloneKVCol
                StandaloneCSMTCol
                StandaloneJournalCol
                StandaloneMetricsCol
                (iso id id)
                prefixedFromKV
                hashHashing
                rtx
                rtx
                (const $ pure ())
    mapM_ (applyKVOnly (kvCommon kvOps) rtx) ops
    action kvOps rtx

runKVOnlyToFull :: [KVOp] -> IO KVOnlyResult
runKVOnlyToFull ops =
    withKVOnly ops $ \kvOps rtx -> do
        Just fullOps <- toFull kvOps
        KVOnlyResult
            <$> rtx (opsRootHash fullOps)
            <*> rtx leafSetT
            <*> rtx kvMapT

runRecoveredKVOnlyToFull :: [KVOp] -> IO KVOnlyResult
runRecoveredKVOnlyToFull ops =
    withKVOnly ops $ \_ rtx -> do
        state <-
            openOps
                []
                2
                100
                StandaloneKVCol
                StandaloneCSMTCol
                StandaloneJournalCol
                StandaloneMetricsCol
                (iso id id)
                prefixedFromKV
                hashHashing
                rtx
                rtx
                (const $ pure ())
        recovered <- case state of
            Ready (ChooseKVOnly kvOps) -> pure kvOps
            Ready (ChooseFull _) ->
                fail "expected ChooseKVOnly"
            NeedsRecovery _ ->
                fail "unexpected sentinel recovery"
        Just fullOps <- toFull recovered
        KVOnlyResult
            <$> rtx (opsRootHash fullOps)
            <*> rtx leafSetT
            <*> rtx kvMapT

applyKVOnly
    :: CommonOps
        Pure
        StandaloneCF
        (Standalone ByteString ByteString Hash)
        StandaloneOp
        ByteString
        ByteString
    -> ( forall b
          . Transaction
                Pure
                StandaloneCF
                (Standalone ByteString ByteString Hash)
                StandaloneOp
                b
         -> IO b
       )
    -> KVOp
    -> IO ()
applyKVOnly common rtx = \case
    Put k v -> rtx $ opsInsert common k v
    Del k -> rtx $ opsDelete common k

fullRoot :: InMemoryDB -> Maybe Hash
fullRoot db =
    fst $ runPure db $ runPureTransaction csmtCodecs rootT

fullLeaves :: InMemoryDB -> LeafSet
fullLeaves db =
    fst $ runPure db $ runPureTransaction csmtCodecs leafSetT

opsForModel :: EquivalentModel -> ([KVOp], [KVOp])
opsForModel (EquivalentModel pairs) =
    ( map (uncurry Put) pairs
    , concatMap path pairs
    )
  where
    path (k, final) =
        [ Put k (oppositePrefix final)
        , Put k final
        ]

    oppositePrefix v =
        let marker =
                case treePrefix prefixedFromKV v of
                    [L] -> 255
                    _ -> 0
        in  B.cons marker (B.drop 1 v)

spec :: Spec
spec =
    describe "CSMT prefix reconciliation properties" $ do
        it
            "P1 model conformance: KVOnly toFull observable state equals Map model"
            $ property
            $ forAllShrink genOps shrinkOps
            $ \ops -> do
                let model = modelOps ops
                KVOnlyResult{kvOnlyLeaves, kvOnlyKV} <-
                    runKVOnlyToFull ops
                kvOnlyKV `shouldBe` model
                kvOnlyLeaves `shouldBe` expectedLeaves model

        it
            "P2 mode equivalence: Full and KVOnly toFull roots and leaves match"
            $ property
            $ forAllShrink genOps shrinkOps
            $ \ops -> do
                let fullDb = runFull ops
                KVOnlyResult{kvOnlyRoot, kvOnlyLeaves} <-
                    runKVOnlyToFull ops
                kvOnlyRoot `shouldBe` fullRoot fullDb
                kvOnlyLeaves `shouldBe` fullLeaves fullDb

        it "P3 no-orphans: leaves and KV values form a bijection"
            $ property
            $ forAllShrink genOps shrinkOps
            $ \ops -> do
                let model = modelOps ops
                KVOnlyResult{kvOnlyLeaves, kvOnlyKV} <-
                    runKVOnlyToFull ops
                kvOnlyKV `shouldBe` model
                Set.size kvOnlyLeaves `shouldBe` Map.size model
                kvOnlyLeaves `shouldBe` expectedLeaves model

        it
            "P4 targeted relocation: 103d753d-style update leaves no old-prefix leaf"
            $ property
            $ forAllShrink genRelocation shrinkRelocation
            $ \(Relocation _ old new) -> do
                let key =
                        "103d753d77c8c541bb912bae22bbaf655ed3bfb177b6598ee0c57720f368cf61#0"
                    ops =
                        [ Put key old
                        , Put key new
                        ]
                    model = modelOps ops
                KVOnlyResult{kvOnlyRoot, kvOnlyLeaves} <-
                    runKVOnlyToFull ops
                oldPrefixLeaves <-
                    withKVOnly ops $ \kvOps rtx -> do
                        Just _ <- toFull kvOps
                        rtx
                            $ collectValues
                                StandaloneCSMTCol
                                []
                                (treePrefix prefixedFromKV old)
                length oldPrefixLeaves `shouldBe` 0
                kvOnlyLeaves `shouldBe` expectedLeaves model
                kvOnlyRoot `shouldBe` fullRoot (runFull ops)

        it
            "P5 crash-recovery determinism: reopened KVOnly journal replays to Full root"
            $ property
            $ forAllShrink genOps shrinkOps
            $ \ops -> do
                let fullDb = runFull ops
                KVOnlyResult{kvOnlyRoot, kvOnlyLeaves} <-
                    runRecoveredKVOnlyToFull ops
                kvOnlyRoot `shouldBe` fullRoot fullDb
                kvOnlyLeaves `shouldBe` fullLeaves fullDb

        it "P6 path independence: equal models yield equal roots"
            $ property
            $ forAllShrink genEquivalentModel shrinkEquivalentModel
            $ \equivalent -> do
                let (ops1, ops2) = opsForModel equivalent
                    model1 = modelOps ops1
                    model2 = modelOps ops2
                model1 `shouldBe` model2
                fullRoot (runFull ops1)
                    `shouldBe` fullRoot (runFull ops2)
