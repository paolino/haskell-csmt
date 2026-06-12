# Manual

!!! note
    The `mts` CLI operates on a **CSMT** instance. MPF does not currently
    have a dedicated CLI. The current hands-on MPF tutorial is the
    [MPF WASM write demo](wasm-mpf-demo.md).

## Demos

For the browser-based tutorials, see:

- [CSMT WASM Verifier Demo](wasm-demo.md)
- [CSMT WASM Write Demo](wasm-write-demo.md)
- [MPF WASM Write Demo](wasm-mpf-demo.md)

### Basic Operations

Insert, query, delete keys and get root hash:

```asciinema-player
{
    "file": "assets/asciinema/basic-ops.cast",
    "idle_time_limit": 2,
    "theme": "monokai",
    "poster": "npt:0:3"
}
```

### Proof Operations

Generate and verify self-contained inclusion proofs:

```asciinema-player
{
    "file": "assets/asciinema/proof-ops.cast",
    "idle_time_limit": 2,
    "theme": "monokai",
    "poster": "npt:0:3"
}
```

## CLI Usage

The MTS package includes a command-line interface (CLI) tool for interacting with the CSMT tree. The CLI provides commands for adding/removing elements, generating proofs, and verifying membership.

CLI works in interactive mode by default. You can also pass commands directly as stdin as we are doing in this manual. In piped mode, status messages are printed in their short form (e.g. `AddedKey` instead of `Added key, inclusion proof generated`).

### Invocation

```text
mts DIR [--csmt-max-files INT] [--kv-max-files INT]
```

| Setting | Env var | Default | Meaning |
| ------- | ------- | ------- | ------- |
| `DIR` (argument) | `CSMT_DB_PATH` | required | Path to the RocksDB database |
| `--csmt-max-files` | `CSMT_MAX_FILES` | 1 | Maximum number of CSMT column files |
| `--kv-max-files` | `KV_MAX_FILES` | 1 | Maximum number of KV column files |

### List of commands
| Command | Description                        | Arguments        | Return value                 |
| ------- | ---------------------------------- | ---------------- | ---------------------------- |
| `i`     | Insert a key-value pair            | key, value       |                              |
| `d`     | Delete a key                       | key              |                              |
| `q`     | Query inclusion proof for a key    | key              | base64 encoding of the proof |
| `r`     | Get the current root of the CSMT   |                  | base64 encoding of the root  |
| `v`     | Verify inclusion proof             | proof            | Valid or Invalid             |
| `w`     | Query value for a key              | key              | value                        |
| `p`     | Query node at partial key          | path (`LRLL...`), optional | node jump path + value hash |
| `k`     | Show a key as tree directions      | key              | `LR...` direction string     |
| `#`     | Comment (no operation)             | free text        |                              |

## Basic operations

### Setup
Setup the environment variable `CSMT_DB_PATH` to point to a directory where the CSMT will store its data. For example:

=== "Input"
    ```bash
    export CSMT_DB_PATH=tmp/demo
    rm -rf $CSMT_DB_PATH
    ```

### Insertion

=== "Input"
    ```bash
    mts <<$
    i key1 value1
    q key1
    $
    ```
=== "Output"
    ```text
    AddedKey
    hJggAAEBAAEAAQEAAQEAAAEAAQABAQEBAAABAAABAQAAAAFYIBCf1UdGHyJjFT9Ie9m6K1UWWQ67U3o15jkbh4ifOE8RgJggAAEBAAEAAQEAAQEAAAEAAQABAQEBAAABAAABAQAAAAE=
    ```

The first line acknowledges the insert; the second is the
base64-encoded CBOR inclusion proof for `key1`. The proof embeds the
hash of the stored value, so it changes when the value changes.

Now the database contains the value for `key1`, and you can query its inclusion proof at any time.

### Verification

=== "Input"
    ```bash
    mts <<$
    v hJggAAEBAAEAAQEAAQEAAAEAAQABAQEBAAABAAABAQAAAAFYIBCf1UdGHyJjFT9Ie9m6K1UWWQ67U3o15jkbh4ifOE8RgJggAAEBAAEAAQEAAQEAAAEAAQABAQEBAAABAAABAQAAAAE=
    $
    ```
=== "Output"
    ```text
    Valid
    ```

The proof is self-contained and includes the value hash, so verification doesn't need the value as a separate argument.

### Querying

Currently you cannot inspect the keys, but you can ask for the values:

=== "Input"
    ```bash
    mts <<< 'w key1'
    ```
=== "Output"
    ```text
    value1
    ```

### Deletion

You can delete keys as well:

=== "Input"
    ```bash
    mts <<< 'd key1'
    ```
=== "Output"
    ```text
    DeletedKey
    ```
Now if you try to query for the inclusion proof of `key1` again:
=== "Input"
    ```bash
    mts <<< 'q key1'
    ```
=== "Output"
    ```text
    KeyNotFound
    ```



Or if you try to get the value for `key1`:

=== "Input"
    ```bash
    mts <<< 'w key1'
    ```
=== "Output"
    ```text
    KeyNotFound
    ```

### Getting the root

You can get the current root of the CSMT tree with the `r` command:

=== "Input"
    ```bash
    mts <<< 'r'
    ```
=== "Output"
    ```text
    TreeEmpty
    ```

If you insert some keys first:

=== "Input"
    ```bash
    mts <<$
    i key1 value1
    r
    i key2 value2
    r
    d key2
    r
    d key1
    r
    $
    ```
=== "Output"
    ```text
    AddedKey
    HZ9W8HqKzlkg3M7y1ivUYtAGm1qJ48zRCU8O3+CCf/A=
    AddedKey
    yZANE8s5l49/d0foRE87Ict/9WZT4zaQjyDyccPmzXE=
    DeletedKey
    HZ9W8HqKzlkg3M7y1ivUYtAGm1qJ48zRCU8O3+CCf/A=
    TreeEmpty
    ```
