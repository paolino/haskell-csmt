# Plan — #169 MPF anchored prefix completeness

Tech: Haskell (mpf-write), hspec + QuickCheck (`unit-tests` suite). Reference:
`CSMT.Proof.Completeness.generateProof` / `CSMT.Core.Completeness.foldCompletenessProof`
build `cpInclusionSteps` (sibling hashes from subtree root → tree root). Port to
MPF over `MPF.Proof.Insertion.MPFProofStep` + `verifyMPFInclusionProof`.

## Slices

### Slice A — trivial full-tree verifier (MERGED, 7dd8ec8)
`verifyMPFCompletenessProof = trustedRoot == foldMPFCompletenessProof`. Correct
only for `prefix = []`. Base case.

### Slice B — anchored prefix completeness (T169-S2)  ← qwen driver, next
The hard problem. Add an anchored proof type + generation + verification:
- `MPFCompletenessProof` = `MPFCompletenessWitness { subtree, anchorSteps :: [MPFProofStep a] }`
  | `MPFCompletenessEmpty (MPFExclusionProof a)` (mirror CSMT).
- `generate prefix`: existing subtree `MPFCompose` + anchor `[MPFProofStep]` from
  the prefix node (subtree root) outward to the full root (reuse the inclusion
  machinery that `mkMPFInclusionProof` uses for branch/fork sibling hashes).
- `verify trustedRoot prefix leaves proof`: fold subtree → subtree root; feed that
  root through the anchor steps (as `verifyMPFInclusionProof` folds a value) →
  full root; `== trustedRoot`; leaf set complete; Empty → `verifyMPFExclusionProof`.
- RED at a **non-`[]`** prefix against the full root; tamper/incomplete/wrong-root.
- Owned: `lib/mpf-write/MPF/Proof/Completeness.hs`, `test/MPF/Proof/CompletenessSpec.hs`,
  `mts.cabal` if a module is added.

### Slice C — byte codec + WASM verifier (T169-S3)
render/parse for `MPFCompletenessProof` + WASM `verify…CompletenessProof`;
`mpf-verify-wasm` opcode `2`, `mpf-write-wasm` `ptCompleteness`.
- Owned: `MPF/Verify.hs`, `MPF/Hashes/*` codec, `app/mpf-*-wasm/Main.hs`.

Order: B → C.
