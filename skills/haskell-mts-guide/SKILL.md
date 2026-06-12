---
name: haskell-mts-guide
description: >-
  Guide for working in the haskell-mts repository (Haskell package
  `mts`): a Merkle tree store with two pluggable trie backends, CSMT
  (compact sparse binary trie) and MPF (16-ary Merkle Patricia Forest,
  Aiken-compatible). Load this when a task mentions haskell-mts, the
  `mts` CLI, MerkleTreeStore, CsmtImpl / MpfImpl, inclusion / exclusion
  / completeness proofs, KVOnly vs Full mode, the journal/replay
  (patchParallel, toFull, toKVOnly, DbState, mergeSubtreeRoots),
  csmtMerkleTreeStore / mpfMerkleTreeStore, fromKVHashes /
  fromHexKVAikenHashes, CSMT_DB_PATH, the RocksDB backend, the WASM
  demos (csmt-verify.wasm, csmt-write.wasm, mpf-write.wasm,
  mpf-verify.wasm), the TypeScript verifier @paolino/csmt-verify, the
  swap-partition rollbacks library, the Lean 4 proofs under lean/, or
  the cabal sublibraries (mts:csmt, mts:mpf, mts:csmt-core,
  mts:csmt-write, mts:csmt-verify, mts:mpf-write, mts:rollbacks).
  Use it to build, test (cabal test unit-tests), format (fourmolu), run
  the CLI, or locate where CSMT/MPF/proof logic lives.
---

# haskell-mts guide

`mts` is a Haskell Merkle tree store with one shared interface and two
trie implementations (CSMT, MPF), plus a rollback library, WASM demos,
and a TypeScript verifier. Everything is one Cabal package split into
sublibraries.

## Repository map

| Path | Sublibrary / role | Purpose |
|------|-------------------|---------|
| `lib/mts/` | `mts` | `MTS.Interface` (the `MerkleTreeStore` GADT, type families, `Mode`), `MTS.Properties` (shared QuickCheck suite), `Data.Serialize.Extra` |
| `lib/csmt-core/` | `mts:csmt-core` | Backend-agnostic CSMT: `CSMT.Core.{Types,Proof,Completeness,Exclusion,Hash,CBOR}`. Pure, WASM-safe. The CBOR wire format lives in `CSMT.Core.CBOR` |
| `lib/csmt-write/` | `mts:csmt-write` | Pure CSMT write path: `CSMT`, `CSMT.Insertion`, `CSMT.Deletion`, `CSMT.Hashes`, `CSMT.MTS` (`csmtMerkleTreeStore`, `Ops` GADT, `DbState`), `CSMT.Populate` (`patchParallel`), `CSMT.Proof.*`, pure/standalone backends |
| `lib/csmt/` | `mts:csmt` | Native add-ons: `CSMT.Backend.RocksDB`, `CSMT.Frontend.CLI.App` (the `mts` CLI). Re-exports all of `csmt-write` |
| `lib/csmt-verify/` | `mts:csmt-verify` | Pure DB-free verification (`CSMT.Verify`, `CSMT.Verify.Blake2b`). Cross-compiles to WASM |
| `lib/mpf-write/` | `mts:mpf-write` | Pure MPF write path: `MPF`, `MPF.Insertion.*`, `MPF.Hashes(.Aiken)`, `MPF.MTS` (`mpfMerkleTreeStore`), `MPF.Proof.*`, `MPF.Verify` (Aiken proof verification) |
| `lib/mpf/` | `mts:mpf` | Native add-on: `MPF.Backend.RocksDB`. Re-exports `mpf-write` |
| `lib/rollbacks/` | `mts:rollbacks` | Swap-partition rollback log (`MTS.Rollbacks.*`). Independent of the tries |
| `app/cli/main.hs` | exe `mts` | CLI entry point (delegates to `CSMT.Frontend.CLI.App`) |
| `app/{csmt,mpf}-{verify,write}-wasm/` | WASM exes | stdio WASM entry points (built only with the `wasm` cabal flag) |
| `app/{test-vectors,fixtures}/` | exes | `csmt-test-vectors`, `csmt-fixtures` generators |
| `test/` | `unit-tests` | hspec-discover suite for CSMT, MPF, shared props, rollbacks |
| `bench/` | benchmarks | `bench`, `populate-bench`, `unified`, `mpf-bench`, `mpf-bench-rocksdb` |
| `lean/` | Lean 4 | Rollback correctness proofs (`Rollbacks/SwapPartition.lean`, `Rollbacks/Rollback.lean`, …) |
| `verifiers/typescript/` | TS | `@paolino/csmt-verify` (CSMT proof verification; not yet on npm) |
| `verifiers/browser*/` | static | HTML/JS for the WASM demos |
| `docs/` | mkdocs | Documentation site source |
| `nix/`, `CI/`, `CD/` | infra | Flake modules, release script, docker compose |

## Build, test, run

Use the nix dev shell and `just`:

```bash
nix develop
just build                 # cabal build all --enable-tests --enable-benchmarks
just test                  # cabal test unit-tests --test-show-details=direct
just test "Exclusion"      # filter specs by pattern
just format / format-check # fourmolu (70-col, leading commas) + cabal-fmt + nixfmt
just lean                  # build Lean proofs
just bench                 # benchmarks
just build-docs            # mkdocs build
```

There is one test-suite, `unit-tests`. Ignore `just test-mpf` and
`just integration` — they name suites that are not declared in
`mts.cabal`.

Build and run the CLI:

```bash
nix build .#mts
export CSMT_DB_PATH=./mydb
./result/bin/mts            # interactive; or pipe commands on stdin
```

WASM modules and demos (x86_64-linux): `nix build .#csmt-verify-wasm`,
`.#csmt-write-wasm`, `.#mpf-verify-wasm`, `.#mpf-write-wasm`,
`.#wasm-artifacts`; preview servers `nix run .#csmt-verify-wasm-demo`,
`.#csmt-wasm-write-demo`, `.#mpf-wasm-write-demo`, `.#docs`.

## Navigating the code

- **Shared interface / where parity is defined**: `MTS.Interface`
  (the `MerkleTreeStore mode imp m` GADT, `MtsKV`/`MtsTree` records,
  `MtsKey`/`MtsValue`/`MtsHash`/`MtsProof`/`MtsLeaf`/`MtsPrefix` type
  families). The parity + replay properties run in
  `test/MTS/PropertySpec.hs` against both backends.
- **CSMT store construction**: `CSMT.MTS.csmtMerkleTreeStore`
  (prefix → runner → `Database` → `FromKV` → `Hashing` → `Full`
  store). KVOnly/lifecycle: `csmtKVOnlyStore`, `csmtManagedTransition`,
  and the `Ops` GADT (`mkKVOnlyOps`, `mkFullOps`, `openOps`,
  `DbState`).
- **MPF store construction**: `MPF.MTS.mpfMerkleTreeStore` (same
  shape, `FromHexKV` + `MPFHashing`). Aiken parity hinges on
  `fromHexKVAikenHashes` / `aikenKeyPath` in `MPF.Hashes`.
- **Proofs**: CSMT inclusion in `CSMT.Proof.Insertion`
  (`buildInclusionProof`, `verifyInclusionProof`, `computeRootHash`),
  exclusion in `CSMT.Core.Exclusion`, completeness in
  `CSMT.Proof.Completeness`. MPF in `MPF.Proof.{Insertion,Exclusion,
  Completeness}` and `MPF.Verify`. CBOR wire format: `CSMT.Core.CBOR`
  (4-element inclusion array; root hash is NOT in the proof).
- **Persistence**: `CSMT.Backend.RocksDB.withRocksDB` and
  `MPF.Backend.RocksDB.withMPFRocksDB`. Four column families: KV, tree,
  journal, metrics (see `*.Backend.Standalone`).
- **Parallel replay / crash recovery**: `CSMT.Populate.patchParallel`,
  `expandToBucketDepth`, `mergeSubtreeRoots`; the journal sentinel and
  `DbState` (`NeedsRecovery` / `Ready`) in `CSMT.MTS`.
- **CLI**: `CSMT.Frontend.CLI.App` — command parser (`i d q v w r p k
  #`), option/env parsing (`CSMT_DB_PATH`, `--csmt-max-files`,
  `--kv-max-files`).

## Using the artifact

The `mts` CLI operates on a CSMT database at `CSMT_DB_PATH`. It runs
interactively or reads commands from stdin (piped mode prints short
status tokens like `AddedKey`). Commands: `i <k> <v>` insert,
`d <k>` delete, `q <k>` inclusion proof (base64 CBOR), `v <proof>`
verify against current root, `w <k>` value lookup, `r` root hash,
`p [path]` node at partial key, `k <k>` key as `L`/`R` directions,
`# …` comment.

```bash
export CSMT_DB_PATH=./mydb
mts <<'EOF'
i key1 value1
q key1
r
EOF
```

Library usage (CSMT, in-memory backend): build a store with
`csmtMerkleTreeStore [] run (pureDatabase csmtCodecs) fromKVHashes
hashHashing`, then `mtsInsert (mtsKV store) k v`,
`mtsMkProof (mtsTree store) k`, `mtsRootHash (mtsTree store)`. The
first argument is the namespace prefix (`[]` = root); every low-level
tree op (`inserting`, `deleting`, `buildInclusionProof`,
`collectValues`, `generateProof`) also takes that prefix first. The
MPF equivalent is `mpfMerkleTreeStore` with `fromHexKVAikenHashes` /
`mpfHashing`. Persistent stores wrap these with `withRocksDB` /
`withMPFRocksDB`. See `docs/library.md` and `docs/interface.md`.

## Answering questions

- **"What is this / how does it work?"** → `README.md` (overview +
  architecture diagram), `docs/index.md`, `docs/concepts.md`.
- **"How do I install / run the CLI?"** → `README.md` Install/Quickstart,
  `docs/installation.md`, `docs/manual.md` (verified CLI transcripts).
- **"How do I use the library / which constructor?"** →
  `docs/interface.md`, `docs/library.md`, and `MTS.Interface` /
  `CSMT.MTS` / `MPF.MTS` source.
- **"CSMT vs MPF differences, proof formats, hashing?"** →
  `docs/csmt.md`, `docs/mpf.md`, `docs/architecture/{inclusion-proof,
  exclusion-proof,storage,example}.md` (CDDL in
  `docs/architecture/*.cddl`).
- **"How are the two backends kept in parity?"** →
  `lib/mts/MTS/Properties.hs` + `test/MTS/PropertySpec.hs`.
- **"Browser / WASM / TypeScript verification?"** →
  `docs/wasm-demo.md`, `docs/wasm-write-demo.md`,
  `docs/wasm-mpf-demo.md`, `docs/typescript.md`,
  `verifiers/typescript/`.
- **"Rollbacks / Lean proofs?"** → `lib/rollbacks/`, `lean/`, and the
  Rollbacks section of `docs/concepts.md`.
- **Release/CI questions** → `.github/workflows/`,
  `release-please-config.json`, `justfile`.

Always verify a claimed command, flag, or module path against the
source before relying on it — the CLI's authoritative command list is
`helpInteractive` in `CSMT.Frontend.CLI.App`.
