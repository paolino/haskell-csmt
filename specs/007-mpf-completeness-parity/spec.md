# Spec — MPF anchored prefix completeness proofs (#169)

## P1 user story

As an MPFS client, I prove the complete key set under an internal-node prefix
against a trusted MPF root — the proof folds the subtree to its root **and**
anchors that root to the tree root — and a browser/WASM verifier accepts honest
proofs and rejects tampered/incomplete sets and wrong roots.

## Context

MPF completeness today folds a subtree to its **own** root. For `prefix = []`
that equals the full root (trivial — recomputing the whole hash). The missing,
useful capability is **anchored prefix completeness**: prove the complete key
set under an *internal-node* prefix against the *full published root*, by
anchoring the subtree root to the tree root via inclusion steps — mirroring
`CSMT.Core.Completeness.CompletenessProof` (`cpMergeOps` + `cpInclusionSteps`).
Empty-prefix reuses the exclusion proof (#148). On-chain Aiken has no
completeness primitive (`has`/`miss` only), so this is off-chain + WASM.

## Acceptance criteria

- [ ] `MPFCompletenessProof` carries the subtree **plus anchor inclusion steps**
      (Witness) or an exclusion proof (Empty), mirroring CSMT's shape.
- [ ] `generate` at an internal-node prefix emits the subtree **and** inclusion
      steps from the subtree root outward to the full tree root.
- [ ] `verify` folds subtree → subtree root → walks anchor steps → full root
      `==` trusted root; claimed leaf set complete; empty-prefix via exclusion.
      **Verifies at a non-`[]` prefix**, not only `[]`.
- [ ] Byte codec + WASM-compilable verifier; `mpf-verify-wasm` opcode `2`,
      `mpf-write-wasm` `ptCompleteness`.
- [ ] Tests: honest internal-node-prefix accepted; extra-leaf, missing-leaf,
      wrong-root rejected; empty-prefix; existing tests green.

## Non-goals

- On-chain Aiken completeness (not a primitive).
- Full-tree (`prefix = []`) as the feature — trivial base case (already shipped).
- CSMT-side changes (CSMT is the reference).
