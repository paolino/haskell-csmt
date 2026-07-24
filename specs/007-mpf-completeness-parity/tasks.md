# Tasks — #169 MPF completeness parity

## Slice A — pure verifyMPFCompletenessProof
- [X] T169-S1 RED: `test/MPF/Proof/CompletenessSpec.hs` asserts
      `verifyMPFCompletenessProof` accepts an honest complete leaf set and
      rejects extra-leaf, missing-leaf, and wrong-root cases; observed failing.
- [X] T169-S1 GREEN: implement + export `verifyMPFCompletenessProof` as the
      trusted-root wrapper over `foldMPFCompletenessProof`, mirroring
      `verifyMPFExclusionProof`; register the spec in the `unit-tests` suite.

## Slice B — Aiken parity
- [ ] T169-S2 completeness proof round-trips through the shared `MPFProofStep`
      Aiken codec; Aiken-side verify accepts honest / rejects tampered; existing
      inclusion+exclusion Aiken bytes unchanged.

## Slice C — WASM opcode
- [ ] T169-S3 `mpf-verify-wasm` opcode `2` verifies a completeness proof;
      `mpf-write-wasm` emits `ptCompleteness`; empty-prefix via exclusion path.
