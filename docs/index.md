!!! warning
    This project is in early development and is not production-ready. Use at your own risk.

# MTS - Merkle Tree Store

## What is MTS?

MTS (Merkle Tree Store) is a Haskell library providing a shared interface for
authenticated key-value stores backed by Merkle tries. It ships with two
implementations:

- **CSMT** - Compact Sparse Merkle Tree: a binary trie with path compression,
  CBOR-encoded inclusion and exclusion proofs, and completeness proofs via
  secondary indexing.
- **MPF** - Merkle Patricia Forest: a 16-ary trie using hex nibble keys,
  with batch/streaming inserts and root hashes compatible with the Aiken
  reference implementation.

Both implementations conform to a single `MerkleTreeStore` GADT indexed by
a `Mode` (`KVOnly` or `Full`), an implementation tag, and a monad, so
application code can be written once and run against either backend.

## Features

- **Shared interface**: mode-indexed `MerkleTreeStore` GADT with type
  families for key, value, hash, proof, leaf, and completeness proof types
  ([MTS Interface](interface.md))
- **Shared QuickCheck suite**: 13 parity properties plus 6 journal/replay
  properties (insert-verify, order independence, completeness round-trip,
  replay idempotence, etc.), each run against both implementations
- **Two trie backends**: Binary (CSMT) and 16-ary (MPF), each with RocksDB
  and in-memory storage
- **Merkle proofs**: Inclusion, exclusion, and completeness proofs for
  both implementations
- **KVOnly fast ingest**: journal-backed mutations with parallel replay
  (`patchParallel`) and crash recovery
- **Batch and streaming inserts**: MPF supports `insertingBatch`,
  `insertingChunked`, and `insertingStream` for large datasets
- **Aiken compatibility**: MPF produces root hashes and proof-step
  encodings matching the Aiken `MerkleTree` implementation (verified
  against the 30-fruit test vector)
- **Rollbacks**: generic swap-partition rollback library
  (`mts:rollbacks`) with Lean 4 correctness proofs under `lean/`
- **Browser demos**: published static demos for read-only CSMT verify,
  CSMT write/prove/verify, and MPF write/prove/verify
- **CLI tool**: Interactive command-line interface for CSMT tree operations
- **TypeScript verifier**: Client-side CSMT proof verification for
  browser/Node.js
- **Pure MPF verifier**: exact Aiken inclusion/exclusion verification in
  Haskell via `MPF.Verify`

## Quick Start

=== "MTS Interface"
    ```haskell
    import MTS.Interface
        ( MtsKV (..), MtsTree (..), mtsKV, mtsTree )

    -- KV ops live in MtsKV, tree ops in MtsTree ('Full mode only)
    example :: MerkleTreeStore 'Full imp IO -> IO ()
    example store = do
        mtsInsert (mtsKV store) "key" "value"
        proof <- mtsMkProof (mtsTree store) "key"
        root  <- mtsRootHash (mtsTree store)
        print (() <$ proof, () <$ root)
    ```

=== "CLI"
    ```bash
    export CSMT_DB_PATH=./mydb
    mts
    > i key1 value1
    Added key, inclusion proof generated
    > r
    root: HZ9W8HqKzlkg3M7y1ivUYtAGm1qJ48zRCU8O3+CCf/A=
    ```

## Status

### Shared Interface (`mts`)
- [x] Mode-indexed `MerkleTreeStore` GADT with type families
- [x] 13 shared parity properties + 6 replay properties
- [x] CSMT passes the full suite
- [x] MPF passes the full suite

### CSMT Implementation (`mts:csmt`)
- [x] Insertion and deletion
- [x] Inclusion and exclusion proof generation and verification (CBOR)
- [x] Completeness proofs (prefix-based subtrees)
- [x] Persistent storage (RocksDB)
- [x] Secondary indexing via `treePrefix`
- [x] KVOnly journal mode with parallel replay and crash recovery
- [x] CLI tool
- [x] TypeScript proof verifier
- [x] Insertion benchmarks

### MPF Implementation (`mts:mpf`)
- [x] Insertion and deletion
- [x] Inclusion and exclusion proof generation
- [x] Completeness proofs (`MPF.Proof.Completeness`)
- [x] Pure Aiken inclusion/exclusion verification (`MPF.Verify`)
- [x] Batch, chunked, and streaming inserts
- [x] Aiken-compatible root hashes and proof-step encoding
- [x] Browser write/prove/verify demo (`mpf-write.wasm` + `mpf-verify.wasm`)
- [x] Persistent storage (RocksDB)
- [x] KVOnly journal mode with replay
- [x] Benchmarks (`mpf-bench`, `mpf-bench-rocksdb`, `unified`)

## Tutorials And Demos

Start here if you want a guided path through the repository:

1. [Installation](installation.md) for local setup and build options
2. [CLI Manual](manual.md) for the CSMT command-line workflow
3. [CSMT WASM Verifier Demo](wasm-demo.md) for the read-only browser verifier
4. [CSMT WASM Write Demo](wasm-write-demo.md) for browser-side mutation +
   proof generation
5. [MPF WASM Write Demo](wasm-mpf-demo.md) for the MPF browser flow with
   Aiken-compatible proofs

### Planned
- [ ] HTTP service with RESTful API
- [ ] Production-grade testing
