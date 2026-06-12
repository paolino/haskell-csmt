# Installation

Releases are cut by release-please; each
[GitHub release](https://github.com/lambdasistemi/haskell-mts/releases)
ships Linux x86_64 artifacts for the `mts` CLI built from the nix flake.

## AppImage bundles

```bash
gh release download --repo lambdasistemi/haskell-mts --pattern '*.AppImage'
chmod +x mts-v*.AppImage
./mts-v*.AppImage --version
```

## Docker images

The tarball loads as `ghcr.io/paolino/mts/mts` with `mts` as entrypoint:

```bash
gh release download --repo lambdasistemi/haskell-mts --pattern '*docker*'
i=$(docker load < mts-v*-docker.tar.gz | sed -e 's/Loaded image: //')
docker run $i --version
```

## DEB packages

```bash
gh release download --repo lambdasistemi/haskell-mts --pattern '*.deb'
sudo dpkg -i mts-v*.deb
```

## RPM packages

```bash
gh release download --repo lambdasistemi/haskell-mts --pattern '*.rpm'
sudo rpm -i mts-v*.rpm
```

## Building from source

You can build with nix

```asciinema-player
{
    "file": "assets/asciinema/bootstrap.cast",
    "idle_time_limit": 2,
    "theme": "monokai",
    "poster": "npt:0:3"
}
```

```bash
nix shell nixpkgs#cachix -c cachix use paolino
nix shell github:lambdasistemi/haskell-mts --refresh
```

Or via cabal provided you have a working Haskell environment and rocksdb development files installed.

```bash
cabal install
```

## WASM Outputs And Preview Commands

The flake exports the combined browser-WASM bundle plus one package per
module (x86_64-linux only, where the GHC WASM toolchain is available):

```bash
nix build .#wasm-artifacts
nix build .#csmt-verify-wasm
nix build .#csmt-write-wasm
nix build .#mpf-verify-wasm
nix build .#mpf-write-wasm
```

It also exports local preview commands for each static bundle:

```bash
PORT=8000 nix run .#csmt-verify-wasm-demo
PORT=8001 nix run .#csmt-wasm-write-demo
PORT=8002 nix run .#mpf-wasm-write-demo
PORT=8003 nix run .#docs
```

## Start With The Tutorials

Once the project builds, the fastest way to understand the current
user-facing behavior is:

1. [CLI Manual](manual.md) for the CSMT command-line workflow
2. [CSMT WASM Verifier Demo](wasm-demo.md) for read-only proof checking
3. [CSMT WASM Write Demo](wasm-write-demo.md) for browser-side mutation
4. [MPF WASM Write Demo](wasm-mpf-demo.md) for MPF build/prove/verify with
   Aiken-compatible proofs
