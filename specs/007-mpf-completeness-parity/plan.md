# Plan — #169 MPF completeness parity

Tech: Haskell (mpf-write lib), hspec + QuickCheck (`mpf-unit-tests` suite),
Aiken proof-step CBOR codec (`MPF.Hashes.Aiken`), wasm32-wasi apps
(`app/mpf-verify-wasm`, `app/mpf-write-wasm`). Reference implementation:
MPF exclusion (`MPF.Proof.Exclusion`, `verifyMPFExclusionProof`) — same shape,
one proof-kind over.

## Slices (bisect-safe, one commit each)

### Slice A — pure `verifyMPFCompletenessProof` (T169-S1)  ← qwen driver, first
Add the trusted-root verifier wrapping the existing
`foldMPFCompletenessProof` (which already computes the root and matches the
leaf set), mirroring `verifyMPFExclusionProof`. RED-first in a new
`MPF.Proof.CompletenessSpec`.
- Owned: `lib/mpf-write/MPF/Proof/Completeness.hs`,
  `test/MPF/Proof/CompletenessSpec.hs`, `mts.cabal` (register the new spec
  module in the `mpf-unit-tests` suite only).

### Slice B — Aiken parity (T169-S2)
Serialize/parse a completeness proof through the shared `MPFProofStep` Aiken
codec; add the Aiken-side verifier + parity vectors. Keep existing
inclusion/exclusion Aiken bytes byte-identical.
- Owned: `lib/mpf-write/MPF/Hashes/Aiken.hs`, `lib/mpf-write/MPF/Verify.hs`,
  completeness test module(s), Aiken parity vectors.

### Slice C — WASM opcode (T169-S3)
Add completeness opcode `2` to `mpf-verify-wasm`; emit `ptCompleteness` from
`mpf-write-wasm`. Empty-prefix routes through the exclusion path.
- Owned: `app/mpf-verify-wasm/Main.hs`, `app/mpf-write-wasm/Main.hs`.

Order: A → B → C. A is the low-blast-radius first dispatch (pure lib + tests).
