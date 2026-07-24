# Spec — MPF completeness (prefix) proofs: Aiken parity + WASM verifier (#169)

## P1 user story

As an MPFS client, I generate a completeness (prefix) proof for a key prefix
and verify it against a trusted MPF root both on-chain (Aiken) and in the
browser (WASM), observing that it accepts the honest complete leaf set and
rejects tampered / incomplete sets.

## Context

MPF completeness proofs exist off-chain (`generateMPFCompletenessProof` /
`foldMPFCompletenessProof`, from #58) but — unlike MPF inclusion and exclusion
(#148) — are not verifiable on-chain (Aiken) or in the browser (WASM). This is
the completeness analogue of #148; it consumes existing generation and adds the
verify / parity / exposure layer. `foldMPFCompletenessProof` already computes
the root and matches the provided leaves; the first-class `verify` wrapper,
Aiken parity, and the WASM opcode are missing.

## Acceptance criteria

- [ ] `verifyMPFCompletenessProof` (pure) verifies a proof against a trusted
      root, mirroring `verifyMPFExclusionProof`; extra-leaf and missing-leaf
      sets fail; wrong root fails.
- [ ] Completeness proof serializes through the shared Aiken `MPFProofStep`
      codec with byte-parity to the on-chain form; existing inclusion/exclusion
      Aiken bytes unchanged.
- [ ] Aiken-side verifier accepts honest completeness proofs and rejects
      tampered ones (parity vectors / round-trip).
- [ ] `mpf-verify-wasm` gains a completeness opcode (`2`) and `mpf-write-wasm`
      emits `ptCompleteness`; browser can verify a prefix proof.
- [ ] Empty-prefix case (no key extends the prefix) verifies via the existing
      MPF exclusion path.
- [ ] Tests: honest verify, tamper, incomplete-set, empty-prefix; existing MPF
      proof / Aiken-parity tests stay green.

## Non-goals

- CSMT-side changes (CSMT already has completeness).
- New off-chain *generation* semantics (exists via #58).
- Prefix-scoped mutation ops (namespace delete already in `MPF.Deletion` /
  `MPF.MTS`).
