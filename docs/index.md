
WARNING: This project is in early development and is not production-ready. Use at your own risk.

# CSMT haskell library and http service

[![GitHub](https://img.shields.io/badge/github-paolino/csmt-blue?logo=github)](https://github.com/paolino/csmt) [![CI](https://github.com/paolino/csmt/actions/workflows/CI.yaml/badge.svg)](https://github.com/paolino/csmt/actions/workflows/CI.yaml)

## What is CSMT?

```asciinema-player
{
"file": "assets/asciinema/bootstrap.cast"
}
```
This package provides:

- A Haskell library implementing a Compact Sparse Merkle Tree (CSMT) data structure with support for persistent storage backends. It offers efficient insertion, deletion, and proof generation functionalities, making it suitable for applications requiring verifiable data structures.
- A CLI tool for interacting with the CSMT, allowing users to perform operations such as adding and removing elements, generating proofs, and verifying membership within the tree.
- An HTTP service that exposes the CSMT functionalities via a RESTful API, enabling remote interaction with the tree for various applications.
- A storage for the preimage of the hashes in sync with the CSMT tree.

## Performance

Preliminary benchmarks indicate that the CSMT library sustains a throughput of 900 insertions per second on a standard development machine over a 3.5M cardano UTxOs dataset.

There is room for optimization via parallel insertions, but these results are promising for an initial implementation.

## Status

- Library
    - [x] Insertion
    - [x] Deletion
    - [x] Proof generation
    - [x] Proof verification
    - [x] Persistent storage backend support
    - [x] Comprehensive tests
    - [x] Insertion benchmarks
    - [ ] Deletion benchmarks
    - [ ] Proof generation benchmarks
    - [ ] Proof verification benchmarks
    - [ ] Production grade tests
- CLI tool
    - [x] Add elements
    - [x] Remove elements
    - [x] Query elements
    - [x] Generate proofs
    - [x] Verify membership
- HTTP service
    - [ ] RESTful API for CSMT operations
    - [ ] Documentation of API endpoints

## Installation

### Docker images

Docker images are available on the CI artifacts. [CI](https://github.com/paolino/csmt/actions/workflows/CI.yaml)

### Arx linux packages

Arx packages are available on the CI artifacts. [CI](https://github.com/paolino/csmt/actions/workflows/CI.yaml)

### Building from source

You can build with nix

```bash
nix shell nixpkgs#cachix -c cachix use paolino
nix shell github:paolino/csmt --refresh
```

Or via cabal provided you have a working Haskell environment and rocksdb development files installed.

```bash
cabal install
```