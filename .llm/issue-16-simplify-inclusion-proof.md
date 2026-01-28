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
- [x] Update `ProofStep` type
- [x] Update `buildInclusionProof`
- [x] Update `foldProof` to take key parameter
- [x] Update `verifyInclusionProof`
- [x] Update serialization in `CSMT/Hashes.hs`
- [x] Update test generators
- [x] Update CLI to require key for verification
- [x] Run tests to verify correctness

## Implementation Notes

Key insight: Since proof steps are ordered leaf-to-root but the key is root-to-leaf,
`foldProof` reverses the key and consumes bits from what was the leaf end first.

The CLI `v` command now requires all three parameters: `v <key> <value> <proof>`
