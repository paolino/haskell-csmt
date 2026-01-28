# CSMT - Compact Sparse Merkle Tree

[![CI](https://github.com/paolino/haskell-csmt/actions/workflows/CI.yaml/badge.svg)](https://github.com/paolino/haskell-csmt/actions/workflows/CI.yaml)
[![Documentation](https://github.com/paolino/haskell-csmt/actions/workflows/deploy-docs.yaml/badge.svg)](https://github.com/paolino/haskell-csmt/actions/workflows/deploy-docs.yaml)

A Haskell library implementing a Compact Sparse Merkle Tree (CSMT) data structure with persistent storage backends.

> **Warning**: This project is in early development and is not production-ready.

## Features

- **Efficient operations**: Insert, delete, and query with logarithmic complexity
- **Merkle proofs**: Generate and verify inclusion proofs
- **Persistent storage**: RocksDB backend for production use
- **Path compression**: Compact representation reduces storage and improves performance
- **Preimage storage**: Automatic storage of key-value preimages alongside the tree

## Quick Start

```haskell
import CSMT
import CSMT.Backend.RocksDB

main :: IO ()
main = withRocksDB "mydb" 256 256 $ \runDB -> do
    -- Insert key-value pairs
    runDB $ runTransaction $ do
        insert fromKVHashes kvCol csmtCol "key1" "value1"
        insert fromKVHashes kvCol csmtCol "key2" "value2"

    -- Get root hash
    mroot <- runDB $ runTransaction $ root csmtCol
    print mroot

    -- Generate inclusion proof (returns value and proof)
    result <- runDB $ runTransaction $
        generateInclusionProof fromKVHashes kvCol csmtCol "key1"
    case result of
        Just (value, proof) -> print proof
        Nothing -> putStrLn "Key not found"
```

## Installation

### Using Nix

```bash
nix shell nixpkgs#cachix -c cachix use paolino
nix shell github:paolino/haskell-csmt --refresh
```

### Using Cabal

Requires a working Haskell environment and RocksDB development files:

```bash
cabal install
```

## CLI Tool

The package includes a CLI for interactive tree operations:

```bash
export CSMT_DB_PATH=./mydb
csmt
> i key1 value1
> q key1
AQDjun1C8tTl1kdY1oon8sAQWL86/UMiJyZFswQ9Sf49XQAA
> r
NrJMih3czFriydMUwvFKFK6VYKZYVjKpKGe1WC4e+VU=
```

## Documentation

Full documentation is available at [paolino.github.io/haskell-csmt](https://paolino.github.io/haskell-csmt/)

## Performance

Preliminary benchmarks show ~900 insertions/second on a standard development machine over a 3.5M Cardano UTxO dataset.

## License

Apache-2.0
