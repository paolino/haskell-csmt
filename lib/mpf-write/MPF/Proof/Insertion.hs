{-# LANGUAGE StrictData #-}

module MPF.Proof.Insertion
    ( MPFProof (..)
    , MPFProofStep (..)
    , MerkleProofItem (..)
    , mkMPFInclusionProof
    , mkMPFNodeInclusionSteps
    , foldMPFProof
    , foldMPFProofFrom
    , verifyMPFInclusionProof
    )
where

import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.List (foldl', isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Database.KV.Transaction
    ( GCompare
    , Selector
    , Transaction
    , query
    )
import MPF.Hashes (MPFHashing (..))
import MPF.Interface
    ( FromHexKV (..)
    , HexDigit (..)
    , HexIndirect (..)
    , HexKey
    )

-- | A single item in a merkle proof
-- Indicates whether this is a left or right sibling in the merkle tree
data MerkleProofItem a
    = -- | Hash of left sibling
      MPILeft a
    | -- | Hash of right sibling
      MPIRight a
    | -- | No sibling (for sparse positions)
      MPINone
    deriving (Show, Eq)

-- | Proof step types for MPF.
--
-- When a branch has exactly one non-empty sibling:
--
-- * If the sibling is a leaf → 'ProofStepLeaf'
-- * If the sibling is a branch → 'ProofStepFork'
--
-- Otherwise → 'ProofStepBranch' with a Merkle proof
-- over all siblings.
data MPFProofStep a
    = ProofStepLeaf
        { pslBranchJump :: HexKey
        -- ^ Branch prefix (skip = length of this)
        , pslOurPosition :: HexDigit
        -- ^ Our nibble at this branch
        , pslNeighborKeyPath :: HexKey
        -- ^ Neighbor leaf's full key path (for CBOR)
        , pslNeighborNibble :: HexDigit
        -- ^ Neighbor leaf's nibble at this branch
        , pslNeighborSuffix :: HexKey
        -- ^ Neighbor leaf's remaining key suffix
        , pslNeighborValueDigest :: a
        -- ^ Neighbor leaf's value digest
        }
    | ProofStepFork
        { psfBranchJump :: HexKey
        -- ^ Branch prefix (skip = length of this)
        , psfOurPosition :: HexDigit
        -- ^ Our nibble at this branch
        , psfNeighborPrefix :: HexKey
        -- ^ Neighbor branch prefix
        , psfNeighborIndex :: HexDigit
        -- ^ Neighbor's position (nibble)
        , psfMerkleRoot :: a
        -- ^ Merkle root of neighbor branch's children
        }
    | ProofStepBranch
        { psbJump :: HexKey
        -- ^ Jump prefix from this branch to next level
        , psbPosition :: HexDigit
        -- ^ Our position (digit) at this branch
        , psbSiblingHashes :: [(HexDigit, a)]
        -- ^ Sibling NODE hashes at this branch
        }
    deriving (Show, Eq)

-- | Complete membership proof for MPF
data MPFProof a = MPFProof
    { mpfProofSteps :: [MPFProofStep a]
    , mpfProofRootPrefix :: HexKey
    , mpfProofLeafSuffix :: HexKey
    -- ^ The remaining key suffix at the leaf level
    , mpfProofValueHash :: a
    -- ^ The hashed value at the leaf
    }
    deriving (Show, Eq)

-- | Generate a membership proof for a key.
-- The prefix scopes the query to a subtree.
--
-- When a branch has exactly one non-empty sibling:
--
-- * If the sibling is a leaf → 'ProofStepLeaf' with
--   full neighbor details (key path, suffix, value)
-- * If the sibling is a branch → 'ProofStepFork' with
--   the neighbor's merkle root
--
-- Otherwise → 'ProofStepBranch' with precomputed
-- node hashes for all siblings (used in Merkle proof).
--
-- This 3-way branching matches the Aiken on-chain
-- validator format byte-for-byte.
mkMPFInclusionProof
    :: (Monad m, GCompare d)
    => HexKey
    -- ^ Prefix (use @[]@ for root)
    -> FromHexKV k v a
    -> MPFHashing a
    -> Selector d HexKey (HexIndirect a)
    -> k
    -> Transaction m cf d ops (Maybe (MPFProof a))
mkMPFInclusionProof prefix FromHexKV{fromHexK} hashing sel k =
    runMaybeT $ do
        let key = fromHexK k
        HexIndirect
            { hexJump = rootJump
            , hexValue = rootValue
            , hexIsLeaf = rootIsLeaf
            } <-
            MaybeT $ query sel prefix
        guard $ isPrefixOf rootJump key
        let remainingAfterRoot = drop (length rootJump) key
        if rootIsLeaf
            then
                pure
                    $ MPFProof
                        { mpfProofSteps = []
                        , mpfProofRootPrefix = []
                        , mpfProofLeafSuffix = rootJump
                        , mpfProofValueHash = rootValue
                        }
            else do
                (steps, leafSuffix, valHash) <-
                    go prefix [] rootJump remainingAfterRoot
                pure
                    $ MPFProof
                        { mpfProofSteps = reverse steps
                        , mpfProofRootPrefix = rootJump
                        , mpfProofLeafSuffix = leafSuffix
                        , mpfProofValueHash = valHash
                        }
  where
    go _ _ _ [] = error "mkMPFInclusionProof: key not found"
    go dbPath logicalPath branchJump (x : ks) = do
        HexIndirect
            { hexJump = childJump
            , hexValue = childValue
            , hexIsLeaf
            } <-
            MaybeT
                $ query
                    sel
                    (dbPath <> branchJump <> [x])
        guard $ isPrefixOf childJump ks
        let remaining = drop (length childJump) ks
        -- Fetch all sibling details (excluding our
        -- position x) and build this branch's step.
        sibDetails <-
            lift
                $ fetchSiblingDetails
                    sel
                    (dbPath <> branchJump)
                    x
        step <-
            lift
                $ buildInclusionStep
                    hashing
                    sel
                    (dbPath <> branchJump)
                    logicalPath
                    branchJump
                    x
                    (Map.toList sibDetails)
        if hexIsLeaf
            then pure ([step], childJump, childValue)
            else do
                let nextDbPath =
                        dbPath <> branchJump <> [x]
                let nextLogicalPath =
                        logicalPath <> branchJump <> [x]
                (restSteps, leafSuffix, valHash) <-
                    go
                        nextDbPath
                        nextLogicalPath
                        childJump
                        remaining
                pure (step : restSteps, leafSuffix, valHash)

-- | Build the inclusion step for a single branch point, given the
-- non-empty siblings (excluding our position).
--
-- Shared by the key-driven 'mkMPFInclusionProof' walk and the
-- node-anchored 'mkMPFNodeInclusionSteps' walk so the branch/fork
-- sibling hashing lives in exactly one place.
buildInclusionStep
    :: (Monad m, GCompare d)
    => MPFHashing a
    -> Selector d HexKey (HexIndirect a)
    -> HexKey
    -- ^ Branch storage path (siblings are queried below this)
    -> HexKey
    -- ^ Logical path consumed so far (for a neighbor leaf's full key)
    -> HexKey
    -- ^ Branch jump
    -> HexDigit
    -- ^ Our position
    -> [(HexDigit, HexIndirect a)]
    -- ^ Non-empty siblings
    -> Transaction m cf d ops (MPFProofStep a)
buildInclusionStep
    hashing
    sel
    branchPath
    logicalPath
    branchJump
    x
    nonEmpty =
        case nonEmpty of
            -- Exactly 1 sibling that is a leaf
            [ ( d
                    , HexIndirect
                        { hexJump = sibSuffix
                        , hexValue = sibVal
                        , hexIsLeaf = True
                        }
                    )
                ] ->
                    let fullKey =
                            logicalPath
                                <> branchJump
                                <> [d]
                                <> sibSuffix
                    in  pure
                            $ ProofStepLeaf
                                { pslBranchJump = branchJump
                                , pslOurPosition = x
                                , pslNeighborKeyPath = fullKey
                                , pslNeighborNibble = d
                                , pslNeighborSuffix = sibSuffix
                                , pslNeighborValueDigest = sibVal
                                }
            -- Exactly 1 sibling that is a branch
            [ ( d
                    , HexIndirect
                        { hexJump = sibPrefix
                        , hexIsLeaf = False
                        }
                    )
                ] -> do
                    mr <-
                        fetchBranchMerkleRoot
                            hashing
                            sel
                            (branchPath <> [d])
                            sibPrefix
                    pure
                        $ ProofStepFork
                            { psfBranchJump = branchJump
                            , psfOurPosition = x
                            , psfNeighborPrefix = sibPrefix
                            , psfNeighborIndex = d
                            , psfMerkleRoot = mr
                            }
            -- Multiple siblings: branch step with Merkle proof
            _ ->
                let nodeHashes =
                        [ (d, computeNodeHash' hashing d')
                        | (d, d') <- nonEmpty
                        ]
                in  pure
                        $ ProofStepBranch
                            { psbJump = branchJump
                            , psbPosition = x
                            , psbSiblingHashes = nodeHashes
                            }

-- | Collect the inclusion steps anchoring an internal node (at
-- @target@) to the tree root scoped by @prefix@.
--
-- Unlike 'mkMPFInclusionProof', which walks root-to-leaf, this walks
-- root-to-node and returns the branch/fork steps from the node
-- outward to the root (node→root order, ready for 'foldMPFProofFrom').
-- Empty when @target@ is the scoped root itself.
mkMPFNodeInclusionSteps
    :: (Monad m, GCompare d)
    => HexKey
    -- ^ Prefix (use @[]@ for root)
    -> MPFHashing a
    -> Selector d HexKey (HexIndirect a)
    -> HexKey
    -- ^ Target internal-node path
    -> Transaction m cf d ops (Maybe [MPFProofStep a])
mkMPFNodeInclusionSteps prefix hashing sel target = runMaybeT $ do
    HexIndirect{hexJump = rootJump} <- MaybeT $ query sel prefix
    if target == prefix
        then pure []
        else reverse <$> goNode prefix [] rootJump
  where
    goNode dbPath logicalPath branchJump = do
        let base = dbPath <> branchJump
        case drop (length base) target of
            [] -> pure []
            (x : _) -> do
                sibDetails <- lift $ fetchSiblingDetails sel base x
                step <-
                    lift
                        $ buildInclusionStep
                            hashing
                            sel
                            base
                            logicalPath
                            branchJump
                            x
                            (Map.toList sibDetails)
                let childDbPath = base <> [x]
                HexIndirect{hexJump = childJump} <-
                    MaybeT $ query sel childDbPath
                if childDbPath == target
                    then pure [step]
                    else
                        (step :)
                            <$> goNode
                                childDbPath
                                (logicalPath <> branchJump <> [x])
                                childJump

-- | Fetch all non-empty sibling details at a branch
-- point (excluding the given digit).
fetchSiblingDetails
    :: (Monad m, GCompare d)
    => Selector d HexKey (HexIndirect a)
    -> HexKey
    -> HexDigit
    -> Transaction m cf d ops (Map HexDigit (HexIndirect a))
fetchSiblingDetails sel pfx exclude = do
    let digits =
            [ HexDigit n
            | n <- [0 .. 15]
            , HexDigit n /= exclude
            ]
    pairs <- mapM fetchOne digits
    pure
        $ Map.fromList
            [(d, hi) | (d, Just hi) <- pairs]
  where
    fetchOne d = do
        mi <- query sel (pfx <> [d])
        pure (d, mi)

-- | Compute the merkle root of a branch's children
-- (16 queries for the child positions).
fetchBranchMerkleRoot
    :: (Monad m, GCompare d)
    => MPFHashing a
    -> Selector d HexKey (HexIndirect a)
    -> HexKey
    -> HexKey
    -> Transaction m cf d ops a
fetchBranchMerkleRoot
    hashing'@MPFHashing{merkleRoot = mr}
    sel
    neighborPath
    neighborPrefix = do
        children <-
            mapM
                fetchChild
                [HexDigit n | n <- [0 .. 15]]
        pure $ mr children
      where
        fetchChild d = do
            mi <-
                query
                    sel
                    ( neighborPath
                        <> neighborPrefix
                        <> [d]
                    )
            pure $ case mi of
                Nothing -> Nothing
                Just hi ->
                    Just
                        $ computeNodeHash' hashing' hi

-- | Compute the node hash from a 'HexIndirect'.
computeNodeHash'
    :: MPFHashing a -> HexIndirect a -> a
computeNodeHash'
    MPFHashing{leafHash = lh}
    HexIndirect{hexJump, hexValue, hexIsLeaf} =
        if hexIsLeaf
            then lh hexJump hexValue
            else hexValue

-- | Fold a proof to compute the root hash.
--
-- Starts with the value hash, computes the leaf hash,
-- then works up through branches.
--
-- Each step type reconstructs a sparse 16-element
-- array differently:
--
-- * 'ProofStepBranch': our hash at our position,
--   sibling hashes at their positions
-- * 'ProofStepLeaf': our hash + one neighbor leaf
--   hash (computed from suffix + value)
-- * 'ProofStepFork': our hash + one neighbor branch
--   hash (computed from prefix + merkle root)
foldMPFProof :: MPFHashing a -> MPFProof a -> a
foldMPFProof
    hashing
    MPFProof{mpfProofSteps, mpfProofLeafSuffix, mpfProofValueHash} =
        foldMPFProofFrom
            hashing
            (leafHash hashing mpfProofLeafSuffix mpfProofValueHash)
            mpfProofSteps

-- | Fold inclusion-proof steps outward from an arbitrary starting
-- node hash.
--
-- Each step combines the accumulator (the node hash at our position)
-- with its siblings to reconstruct the parent branch hash.
-- 'foldMPFProof' is the special case whose starting node is a leaf
-- (its hash computed via 'leafHash'); an anchored completeness proof
-- starts from an internal subtree-root hash instead.
foldMPFProofFrom :: MPFHashing a -> a -> [MPFProofStep a] -> a
foldMPFProofFrom hashing = foldl' (applyProofStep hashing)

applyProofStep :: MPFHashing a -> a -> MPFProofStep a -> a
applyProofStep hashing acc proofStep =
            case proofStep of
                ProofStepBranch
                    { psbJump
                    , psbPosition
                    , psbSiblingHashes
                    } ->
                        let siblingMap =
                                Map.fromList
                                    psbSiblingHashes
                            sparseArray =
                                [ if HexDigit n
                                    == psbPosition
                                    then Just acc
                                    else
                                        Map.lookup
                                            (HexDigit n)
                                            siblingMap
                                | n <- [0 .. 15]
                                ]
                            mr =
                                merkleRoot
                                    hashing
                                    sparseArray
                        in  branchHash
                                hashing
                                psbJump
                                mr
                ProofStepLeaf
                    { pslBranchJump
                    , pslOurPosition
                    , pslNeighborNibble
                    , pslNeighborSuffix
                    , pslNeighborValueDigest
                    } ->
                        let neighborHash =
                                leafHash
                                    hashing
                                    pslNeighborSuffix
                                    pslNeighborValueDigest
                            sparseArray =
                                [ if HexDigit n
                                    == pslOurPosition
                                    then Just acc
                                    else
                                        if HexDigit n
                                            == pslNeighborNibble
                                            then
                                                Just
                                                    neighborHash
                                            else Nothing
                                | n <- [0 .. 15]
                                ]
                            mr =
                                merkleRoot
                                    hashing
                                    sparseArray
                        in  branchHash
                                hashing
                                pslBranchJump
                                mr
                ProofStepFork
                    { psfBranchJump
                    , psfOurPosition
                    , psfNeighborPrefix
                    , psfNeighborIndex
                    , psfMerkleRoot
                    } ->
                        let neighborHash =
                                branchHash
                                    hashing
                                    psfNeighborPrefix
                                    psfMerkleRoot
                            sparseArray =
                                [ if HexDigit n
                                    == psfOurPosition
                                    then Just acc
                                    else
                                        if HexDigit n
                                            == psfNeighborIndex
                                            then
                                                Just
                                                    neighborHash
                                            else Nothing
                                | n <- [0 .. 15]
                                ]
                            mr =
                                merkleRoot
                                    hashing
                                    sparseArray
                        in  branchHash
                                hashing
                                psfBranchJump
                                mr

-- | Verify a membership proof.
-- The prefix scopes the root query to a subtree.
verifyMPFInclusionProof
    :: (Eq a, Monad m, GCompare d)
    => HexKey
    -- ^ Prefix (use @[]@ for root)
    -> FromHexKV k v a
    -> Selector d HexKey (HexIndirect a)
    -> MPFHashing a
    -> v
    -> MPFProof a
    -> Transaction m cf d ops Bool
verifyMPFInclusionProof
    prefix
    FromHexKV{fromHexV}
    sel
    hashing@MPFHashing{leafHash = lh}
    v
    proof = do
        let valueHash = fromHexV v
        mv <- query sel prefix
        pure $ case mv of
            Just
                HexIndirect
                    { hexJump
                    , hexValue
                    , hexIsLeaf
                    } ->
                    let rootNodeHash =
                            if hexIsLeaf
                                then lh hexJump hexValue
                                else hexValue
                    in  mpfProofValueHash proof == valueHash
                            && rootNodeHash
                                == foldMPFProof hashing proof
            Nothing -> False
