# Encodings

## Jumps (keys) encoding

Jumps are just key infixes. Technically they are bitstring of any length.

Because we do not plan to handle key lenghts of more than 65536 bits we use a word16 to encode the lenght of the jump in bits, followed by the bits packed in bytes.
We stick to big-endian encoding for the word16 and we store the bits left-aligned in the bytes.


| Jump        | Encoding            |
| ----------- | ------------------- |
|           | 0x00 0x00           |
| L         | 0x00 0x01 0x00      |
| R         | 0x00 0x01 0x80      |
| LL        | 0x00 0x02 0x00      |
| LR        | 0x00 0x02 0x40      |
| RL        | 0x00 0x02 0x80      |
| RR        | 0x00 0x02 0xc0      |
| LLLLLLLL  | 0x00 0x08 0x01      |
| RRRRRRRR  | 0x00 0x08 0xfe      |
| LLLLLLLLL | 0x00 0x09 0x00 0x00 |
| RRRRRRRRR | 0x00 0x09 0xff 0x80 |
| RRRRRRRRL | 0x00 0x09 0xff 0x00 |


Encoding the full jump length covers also the bytestring length.

```haskell
byteslenght len =
    let (l, r) = len `divMod` 8
    in if r == 0 then l else l + 1
```

## Bytestring encoding

Bytestrings are stored as their length in a word16 big-endian followed by the bytes.


| Bytestring | Encoding                           |
| ---------- | ---------------------------------- |
|          | 0x00 0x00                          |
| a        | 0x00 0x01 0x61                     |
| abc      | 0x00 0x03 0x61 0x62 0x63           |
| hello    | 0x00 0x05 0x68 0x65 0x6c 0x6c 0x6f |


### Hash encoding

Hashes are considered variable-length bytestring

## Node encoding

Nodes are encoded as the concatenation of the jump encoding followed by the hash encoding.


| Node                              | Encoding                                     |
| --------------------------------- | -------------------------------------------- |
| `{jump : "", value : "abc"}`      | 0x00 0x00 0x00 0x03 0x61 0x62 0x63           |
| `{jump : "LRL", value : "hello"}` | 0x00 0x03 0x40 0x00 0x04 0x64 0x61 0x74 0x61 |