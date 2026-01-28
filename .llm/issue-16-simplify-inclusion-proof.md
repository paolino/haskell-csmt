# Issue #16: Simplify Inclusion Proof Format

## Summary

The `ProofStep` type contains redundant information that can be derived from the key being verified. This issue simplifies the proof format to only store essential data.

## Current Implementation

```haskell
data ProofStep a = ProofStep
    { stepDirection :: Direction      -- REDUNDANT: derivable from key
    , stepJump :: Key                 -- PARTIALLY REDUNDANT: only length needed
    , stepSibling :: Indirect a       -- NEEDED: sibling value
    }
```

## Problem Analysis

During verification in `foldProof`:
1. `stepDirection` - The direction at each step is determined by the current bit of the key
2. `stepJump` - The jump path content is the next N bits of the key; only N (length) is needed
3. `stepSibling` - Required for computing the parent hash

## Proposed Solution

```haskell
data ProofStep a = ProofStep
    { stepConsumed :: Int       -- Length of key consumed (1 for direction + N for jump)
    , stepSibling :: Indirect a -- Sibling value
    }
```

Verification works by:
1. Take the key being proven
2. At each step, consume `stepConsumed` bits
3. First bit = direction, remaining bits = jump path
4. Use direction + reconstructed jump to combine with sibling

## Files to Modify

1. **`src/csmt/CSMT/Proof/Insertion.hs`** - Core change
   - Update `ProofStep` data type
   - Update `buildInclusionProof` to compute `stepConsumed = 1 + length jump`
   - Update `foldProof` to take the key and derive direction/jump from it
   - Update `verifyInclusionProof` to pass key to `foldProof`

2. **`src/csmt/CSMT/Hashes.hs`** - Serialization
   - Update `putProof` - serialize `stepConsumed` as Word16 instead of direction + key
   - Update `getProof` - deserialize new format

3. **`test/CSMT/HashesSpec.hs`** - Tests
   - Update `genProofs` generator for new `ProofStep` format

## Verification Algorithm Change

Old `foldProof`:
```haskell
foldProof :: Hashing a -> a -> Proof a -> a
```

New `foldProof`:
```haskell
foldProof :: Hashing a -> Key -> a -> Proof a -> a
```

The key is now required to reconstruct direction and jump at each step.

## Backwards Compatibility

This is a **breaking change** to:
- The `ProofStep` data type (API change)
- The proof serialization format (wire format change)

Existing serialized proofs will not be compatible.

## Implementation Progress

- [x] Investigation complete
- [x] Rename `Proof` to `InclusionProof`
- [x] Add `proofKey`, `proofValue`, `proofRootHash` fields
- [x] Update `ProofStep` type (stepConsumed + stepSibling only)
- [x] Update `buildInclusionProof` to populate all fields
- [x] Make `verifyInclusionProof` pure (no database access needed)
- [x] Add `computeRootHash` helper function
- [x] Update serialization in `CSMT/Hashes.hs` (using CBOR)
- [x] Update test generators
- [x] Simplify CLI `v` command to just `v <proof>`
- [x] Run tests to verify correctness

## Final Implementation

The `InclusionProof` is now fully self-contained:

```haskell
data InclusionProof a = InclusionProof
    { proofKey :: Key        -- The key being proven
    , proofValue :: a        -- The value at the key
    , proofRootHash :: a     -- The root hash this proves against
    , proofSteps :: [ProofStep a]
    , proofRootJump :: Key
    }
```

Verification is now pure:
```haskell
verifyInclusionProof :: Eq a => Hashing a -> InclusionProof a -> Bool
```

The verifier just needs to trust that `proofRootHash` matches a known/trusted root.

## API Investigation: Value vs Hash Parameter

### Question
Should `buildInclusionProof` require the raw value `v` or just the hash `a`?

### Findings

1. **How the value is used in proof generation**:
   ```haskell
   buildInclusionProof FromKV{fromK, fromV} sel hashing k v = runMaybeT $ do
       let key = fromK k
           value = fromV v   -- v is ONLY used here to compute hash
       ...
           , proofValue = value  -- hash goes into proof
   ```
   The raw value `v` is only passed to `fromV` to compute the hash. The proof stores the hash, not the raw value.

2. **API comparison between branches**:
   | Branch | `generateInclusionProof` signature |
   |--------|-----------------------------------|
   | main | `FromKV -> Selector -> k -> Transaction ...` (looks up value from DB) |
   | issue-16 | `FromKV -> Selector -> k -> v -> Transaction ...` (value provided by caller) |

3. **cardano-utxo-csmt usage** (uses main branch):
   ```haskell
   proofBytes <- generateInclusionProof fromKVLazy CSMTCol txIn
   txOut <- query KVCol txIn   -- value queried separately for response
   ```
   The main branch API internally looks up the value from the database.

### Design Options

**Option A: Keep raw value `v` parameter (current issue-16 approach)**
- Pros: Consistent API, library handles hashing
- Cons: Caller must have raw value available

**Option B: Change to hash `a` parameter**
- Pros: More flexible, works if caller only has hash
- Cons: Requires caller to know about `fromV` for hashing

**Option C: Provide both APIs**
- `buildInclusionProof :: ... -> k -> a -> ...` (takes hash)
- `buildInclusionProofFromValue :: ... -> k -> v -> ...` (takes raw value, hashes it)

### Conclusion
Changed to **Option D**: Look up value from KV column and return both.

The API now:
```haskell
buildInclusionProof
    :: (Monad m, Ord k, GCompare d)
    => FromKV k v a
    -> Selector d k v           -- KV column to look up value
    -> Selector d Key (Indirect a)  -- CSMT column
    -> Hashing a
    -> k
    -> Transaction m cf d ops (Maybe (v, InclusionProof a))
```

**Rationale**: Only generate proofs for what's actually in the tree NOW. This ensures
consistency - the proof always matches the current state. The caller gets both the
value and proof, knowing they're consistent.

**cardano-utxo-csmt migration**: Simple change - just remove the separate `query` call:
```haskell
-- Before:
proofBytes <- generateInclusionProof fromKVLazy CSMTCol txIn
txOut <- query KVCol txIn

-- After:
result <- generateInclusionProof fromKVLazy KVCol CSMTCol txIn
-- result is Maybe (txOut, proofBytes)
```
