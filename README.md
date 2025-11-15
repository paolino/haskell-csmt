# CSMT

This package provides:
- A Haskell library implementing a Compact Sparse Merkle Tree (CSMT) data structure with support for persistent storage backends. It offers efficient insertion, deletion, and proof generation functionalities, making it suitable for applications requiring verifiable data structures.
- A CLI tool for interacting with the CSMT, allowing users to perform operations such as adding and removing elements, generating proofs, and verifying membership within the tree.
- An HTTP service that exposes the CSMT functionalities via a RESTful API, enabling remote interaction with the tree for various applications.

This package does not provide a storage for the preimage of the hashes; it is designed to be storage-agnostic, allowing users to integrate their preferred persistent storage solutions. It will compute the hashes of key and values and store those in the tree, but it is up to the user to manage the actual key-value pairs externally.