# Library API

This page covers using the CSMT library in Haskell applications.

## Overview

The library provides:

- `CSMT` - Main module re-exporting the public API
- `CSMT.Hashes` - Blake2b-256 based operations
- `CSMT.Backend.RocksDB` - Persistent storage backend
- `CSMT.Backend.Pure` - In-memory backend for testing

## Basic Setup

### With RocksDB (Production)

```haskell
import CSMT
import CSMT.Hashes
import CSMT.Backend.RocksDB
import CSMT.Backend.Standalone

-- Open database and run operations
main :: IO ()
main = withRocksDB "path/to/db" 256 256 $ \(RunRocksDB runDB) -> do
    db <- runDB $ standaloneRocksDBDatabase codecs
    -- Use db for transactions
```

### With Pure Backend (Testing)

```haskell
import CSMT
import CSMT.Backend.Pure
import CSMT.Backend.Standalone

-- Run in-memory operations
example :: (result, InMemoryDB)
example = runPure emptyInMemoryDB $ do
    runPureTransaction codecs $ do
        -- Your operations here
```

## Core Operations

### Inserting Values

```haskell
import CSMT.Hashes (insert, fromKVHashes)

-- Insert a key-value pair
insertExample :: Transaction m cf d ops ()
insertExample =
    insert fromKVHashes kvCol csmtCol "mykey" "myvalue"
```

The `insert` function:

1. Stores the key-value pair in the KV column
2. Computes the value hash
3. Updates the CSMT structure
4. Recomputes affected node hashes

### Deleting Values

```haskell
import CSMT.Hashes (delete, fromKVHashes)

-- Delete a key
deleteExample :: Transaction m cf d ops ()
deleteExample =
    delete fromKVHashes kvCol csmtCol "mykey"
```

Deletion:

1. Removes the key from the KV column
2. Updates the tree structure (may compact nodes)
3. Recomputes affected hashes

### Querying the Root

```haskell
import CSMT.Hashes (root)

-- Get current root hash
getRootExample :: Transaction m cf d ops (Maybe ByteString)
getRootExample = root csmtCol
```

Returns `Nothing` if the tree is empty.

## Merkle Proofs

### Generating Inclusion Proofs

```haskell
import CSMT.Hashes (generateInclusionProof, fromKVHashes)

-- Generate proof for a key
proofExample :: Transaction m cf d ops (Maybe ByteString)
proofExample =
    generateInclusionProof fromKVHashes csmtCol "mykey"
```

Returns `Nothing` if the key doesn't exist.

### Verifying Inclusion Proofs

```haskell
import CSMT.Hashes (verifyInclusionProof, fromKVHashes)

-- Verify a proof
verifyExample :: ByteString -> Transaction m cf d ops Bool
verifyExample proofBytes =
    verifyInclusionProof fromKVHashes csmtCol "expectedValue" proofBytes
```

Returns `True` if the proof is valid for the given value.

## Custom Key/Value Types

The library supports custom types via `FromKV`:

```haskell
import CSMT.Interface (FromKV(..))

-- Define conversion for your types
myFromKV :: FromKV MyKey MyValue Hash
myFromKV = FromKV
    { fromK = myKeyToPath    -- Convert key to tree path
    , fromV = myValueToHash  -- Convert value to hash
    }
```

## Codecs

For storage, define codecs using Prisms:

```haskell
import CSMT.Backend.Standalone (StandaloneCodecs(..))
import Control.Lens (prism')

myCodecs :: StandaloneCodecs MyKey MyValue Hash
myCodecs = StandaloneCodecs
    { keyCodec = prism' encodeKey decodeKey
    , valueCodec = prism' encodeValue decodeValue
    , nodeCodec = prism' encodeHash decodeHash
    }
```

## Column Selectors

Operations use type-safe column selectors:

```haskell
import CSMT.Backend.Standalone (Standalone(..))
import Database.KV.Transaction (Selector, sel)

-- KV column selector
kvCol :: Selector (Standalone k v a) k v
kvCol = sel StandaloneKVCol

-- CSMT column selector
csmtCol :: Selector (Standalone k v a) Key (Indirect a)
csmtCol = sel StandaloneCSMTCol
```

## Error Handling

Most operations return `Maybe` or are in a `Transaction` monad:

- `Nothing` typically means "key not found"
- Invalid proofs return `False` from verification
- Database errors surface as exceptions

## Performance Tips

1. **Batch operations**: Group multiple inserts/deletes in a single transaction
2. **Column family tuning**: Adjust `maxFiles` parameters for your workload
3. **Parallel insertion**: Future versions will support parallel batch inserts
