# Repository Agent Guide

## What this repo is

`haskell-mts` is a Haskell library (package name `mts`) providing a
shared Merkle tree store interface with two pluggable trie backends:
**CSMT** (a compact sparse binary trie) and **MPF** (a 16-ary Merkle
Patricia Forest with Aiken-compatible hashing). Both expose the same
mode-indexed `MerkleTreeStore` GADT and pass a shared QuickCheck suite.
The repo also ships a generic swap-partition rollback library
(`mts:rollbacks`) with Lean 4 correctness proofs, WASM builds of the
pure write/verify paths with in-browser demos, an interactive `mts`
CLI for CSMT, and a TypeScript CSMT proof verifier.

It is split into Cabal sublibraries (see `mts.cabal`): `mts` (interface
+ properties), `csmt-core`, `csmt-write`, `csmt`, `csmt-verify`,
`mpf-write`, `mpf`, `rollbacks`, plus test-lib helpers.

## How to work here

The nix dev shell provides GHC, cabal, just, fourmolu, mkdocs, and
asciinema tooling. Prefer `just` recipes over raw cabal.

```bash
nix develop                 # enter the dev shell
just build                  # cabal build all --enable-tests --enable-benchmarks
just test                   # cabal test unit-tests (add "pattern" to filter)
just test "MPF"             # run only matching specs
just format                 # fourmolu + cabal-fmt + nixfmt
just format-check           # CI formatting gate
just lean                   # build the Lean 4 proofs (cd lean && lake build)
just bench                  # run benchmarks
just serve-docs             # mkdocs live preview
just build-docs             # mkdocs build
```

There is a single test-suite, `unit-tests` (`cabal test unit-tests`),
covering CSMT, MPF, the shared properties, and rollbacks. (The
`just test-mpf` / `just integration` recipes reference test-suites that
do not exist in `mts.cabal` — use `just test` instead.)

Build the CLI and run it:

```bash
nix build .#mts
export CSMT_DB_PATH=./mydb
./result/bin/mts <<< 'i k v'
```

WASM artifacts and demos are exported by the flake on x86_64-linux
(`nix build .#csmt-verify-wasm`, `nix run .#docs`, etc.).

## Skills

Activatable procedures live under `skills/`. Load the one whose
description matches your task:

- `skills/haskell-mts-guide/` — how the repo is laid out, how to build
  and test it, where the CSMT/MPF/rollbacks logic lives, how to use the
  `mts` CLI and the library API, and where answers to common questions
  live.

## Conventions

- Apache-2.0 licensed; keep every component Hackage-shaped where
  possible (`cabal check`), though `rocksdb-kv-transactions` is not yet
  on Hackage.
- Fourmolu formatting with a 70-character line limit, leading commas.
- Releases are cut by release-please (manifest mode); `mts.cabal`'s
  version block is kept in sync and CI fails on drift.
