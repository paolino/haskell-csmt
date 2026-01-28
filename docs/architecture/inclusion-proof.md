# Inclusion Proof Format

Inclusion proofs allow verifying that a key-value pair exists in a CSMT without
access to the full tree. Proofs are serialized using CBOR for compact,
portable representation.

## Overview

An inclusion proof is self-contained: it includes all information needed to
verify membership, including the key, value hash, and expected root hash.
Verification is pure computation with no database access required.

## CDDL Specification

The proof format is formally specified in CDDL (Concise Data Definition Language):

```cddl
--8<-- "docs/architecture/inclusion-proof.cddl"
```

## Structure

### Direction

A single bit indicating left (0) or right (1) in the tree traversal.

### Key

A path through the tree represented as a list of directions. For Blake2b-256
hashed keys, this is typically 256 directions (one per bit of the hash).

### Hash

A 32-byte Blake2b-256 hash value.

### Indirect

A tree node reference containing:

- **jump**: Key prefix that can be skipped (optimization for sparse trees)
- **value**: Hash of the node

### ProofStep

Each step in the proof contains:

- **step_consumed**: Number of key bits consumed at this level (1 for direction
  plus length of any jump)
- **step_sibling**: The sibling node needed to reconstruct the parent hash

### InclusionProof

The complete proof contains:

| Field | Description |
|-------|-------------|
| `proof_key` | The key being proven |
| `proof_value` | Hash of the value at the key |
| `proof_root_hash` | Root hash this proof validates against |
| `proof_steps` | List of steps from leaf to root |
| `proof_root_jump` | Jump path at the root node |

## Verification

To verify a proof:

1. Start with `proof_value` as the current hash
2. For each step, combine the current hash with `step_sibling` based on the
   direction derived from `proof_key`
3. Apply the root jump
4. Compare the computed root hash with `proof_root_hash`

The verifier must independently trust `proof_root_hash` (e.g., from a trusted
source or blockchain).

## Example

```
Key:       [L, R, L, L, ...]  (256 directions)
Value:     0xabc123...        (32-byte hash)
Root Hash: 0xdef456...        (32-byte hash)
Steps:     [{consumed: 3, sibling: ...}, ...]
```

The proof demonstrates that following the key path through the tree, combining
hashes at each level with the provided siblings, produces the stated root hash.
