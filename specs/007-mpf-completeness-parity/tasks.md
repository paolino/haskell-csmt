# Tasks — #169 MPF anchored prefix completeness

## Slice A — trivial full-tree verifier (MERGED)
- [X] T169-S1 RED: CompletenessSpec asserts verifyMPFCompletenessProof over the
      full tree (accept honest; reject extra-leaf, missing-leaf, wrong-root).
- [X] T169-S1 GREEN: verifyMPFCompletenessProof = trustedRoot == fold; registered
      in the `unit-tests` suite.

## Slice B — anchored prefix completeness (the hard problem)
- [X] T169-S2 RED: CompletenessSpec proves the complete key set under a
      **non-`[]` internal-node prefix** against the **full** root, and rejects
      extra-leaf, missing-leaf, tampered-anchor, and wrong-root; observed failing.
- [X] T169-S2 GREEN: `MPFCompletenessProof` (Witness{subtree, anchorSteps} |
      Empty exclusion); `generate` emits subtree + anchor steps to root; `verify`
      folds subtree → anchor → full root; empty-prefix via exclusion.

## Slice B2 — property tests for anchored completeness (expect a RED bug)
- [ ] T169-S4 RED: QuickCheck property tests (random tree + random internal-node
      prefix, mirror ExclusionSpec generators) — honest verifies against the full
      root; tampered/extra/missing rejects; absent prefix via exclusion. Observed
      (fail with counterexample, or pass = coverage-only). No coverage-gating.
- [ ] T169-S4 GREEN (only if a bug surfaced): fix the anchor
      (Completeness.hs / Insertion.hs helpers) so the property holds — without
      weakening the property or narrowing the generator.

## Slice C — byte codec + WASM verifier
- [ ] T169-S3 render/parse codec + WASM-compilable verifier; `mpf-verify-wasm`
      opcode `2`; `mpf-write-wasm` `ptCompleteness`; empty-prefix via exclusion.
