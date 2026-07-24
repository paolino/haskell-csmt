#!/usr/bin/env bash
# PR-lifetime gate for haskell-mts#169 (MPF completeness parity).
# Present = PR in flight; dropped in a `chore: drop gate.sh` commit at finalize.
set -euo pipefail

nix develop --accept-flake-config -c bash -c '
  set -euo pipefail
  cabal build all --enable-tests
  cabal test mpf-unit-tests --test-show-details=direct
  cabal test unit-tests --test-show-details=direct
'
