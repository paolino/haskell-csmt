# System Overview

The CSMT system provides multiple interfaces for interacting with the tree.

## Current Architecture

```mermaid
graph TD
    L[CSMT CLI] -->|CSMT Operations| C[CSMT Library]
    H[Haskell Application] -->|CSMT Operations| C
    C -->|Read/Write Nodes & Preimages| D[RocksDB Storage]
```

A CSMT instance consists of:

- **RocksDB Storage**: Persistent backend for tree nodes and preimages
- **CSMT Library**: Core Haskell implementation of the data structure
- **CLI Tool**: Interactive command-line interface for tree operations

## Planned Architecture

```mermaid
graph TD
    A[Client] -->|HTTP Requests| B[CSMT HTTP Service]
    B -->|CSMT Operations| C[CSMT Library]
    L[CSMT CLI] -->|CSMT Operations| C
    H[Haskell Application] -->|CSMT Operations| C
    C -->|Read/Write Nodes & Preimages| D[RocksDB Storage]
```

A future HTTP service will expose the CSMT functionalities via a RESTful API.


