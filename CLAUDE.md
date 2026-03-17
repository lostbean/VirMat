# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

VirMat is a Haskell monorepo for generating 2D/3D virtual microstructures for metallurgical research. It produces Voronoi-based grain structures with realistic size distributions and crystallographic textures, outputting VTK and ANG files.

## Build & Development

**Prerequisites:** Nix (provides GHC 9.6.x, Stack, Cabal, HLS, Lefthook, formatters).

```bash
nix develop                                    # Enter dev shell
stack build                                    # Build entire monorepo
stack build <package>                          # Build single package (e.g. stack build sledge)
stack exec virmatgen -- --help                 # Run the CLI executable
```

CI runs: `nix fmt -- --ci` (format check) then `nix develop --command stack build --system-ghc`.

## Testing

Tests are gated behind cabal flags (default: off). To run:

```bash
stack test sledge --flag sledge:tests          # sledge test suite (tasty + QuickCheck)
stack test queryforest --flag queryforest:test  # queryforest tests
stack build DeUni --flag DeUni:test            # DeUni test executables (not a test-suite, uses diagrams/SVG)
stack build hammer --flag hammer:test          # hammer benchmark/profile executables
```

## Formatting

Formatting is enforced by Lefthook pre-commit hooks and CI. Three formatters via treefmt.nix:
- **Fourmolu** — Haskell (.hs)
- **cabal-fmt** — Cabal files (.cabal)
- **nixpkgs-fmt** — Nix files (.nix)

```bash
nix fmt                    # Format all files
nix fmt -- --ci            # Check formatting without modifying (CI mode)
```

## Monorepo Structure

All packages are git submodules under `packages/`. The root `stack.yaml` (resolver: lts-22.11, `allow-newer: true`) ties them together.

### Package Dependency Graph

```
VirMat (root executable + library)
├── DeUni          — Delaunay triangulation (DeWall/MBC algorithm)
├── SubZero        — Subdivision surfaces (Loop scheme)
│   ├── hammer
│   └── linear-vect
├── hammer         — Microstructure graph, grain topology, sparse matrices
│   ├── linear-vect
│   ├── mcl
│   └── VTK
├── sledge         — Crystallography: rotations, Bingham distributions, EBSD I/O
│   ├── hammer
│   ├── linear-vect
│   └── queryforest
├── queryforest    — Spatial indexing: KD-trees, VP-trees
├── linear-vect    — Vec2/3/4, Mat2/3/4, quaternions (BSD-3-Clause fork)
├── mcl            — Markov Cluster Algorithm
└── VTK            — VTK XML file generation
```

### Generation Pipeline (root library)

The `virmatgen` executable dispatches to `VirMat.Run2D` or `VirMat.Run3D` based on `--2d`/`--3d` flags. The pipeline is:

1. **Sampling** (`Core.Sampling`) — Draw grain sizes from LogNormal/Normal/Uniform/Custom distributions via inverse-CDF
2. **Packing** (`Core.Packer`) — Pack spheres/circles using Verlet integration
3. **Voronoi** (`Core.VoronoiMicro`) — Build Delaunay triangulation (DeUni), extract Voronoi dual
4. **Subdivision** (`Core.FlexMicro`) — Apply Loop subdivision surfaces (SubZero) for smooth grain boundaries
5. **Texture** (`Distributions.Texture.ODFSampling`) — Sample crystallographic orientations via Bingham distributions (sledge)
6. **Export** (`IO.Export`) — Write VTK files and ANG grids with rasterized triangle data

## Development Approach

Take a **TDD (Test-Driven Development)** approach: write or update tests first, verify they fail, then implement the change, then verify tests pass. When fixing bugs, first write a test that reproduces the bug before writing the fix.

## Git Conventions

- **No footer on commit messages.** Do not add `Co-Authored-By` or any other footer lines.
- Keep commit messages concise and descriptive.

## Submodule Workflow

Packages are git submodules under `packages/`. Submodules are typically in **detached HEAD** state after checkout.

### Committing changes to a submodule

1. `cd packages/<name>`
2. Stage and commit your changes (this creates a commit on the detached HEAD)
3. Update the local master branch to include your commit: `git branch -f master HEAD`
4. Return to root and commit the updated submodule ref

### Pushing

Push submodules **before** the root, since the root records submodule commit refs:

```bash
# For each changed submodule:
cd packages/<name>
git branch -f master HEAD        # Point master at the detached HEAD commit
git push origin master
cd ../..

# Then push root:
git push origin master
```

### Pulling

```bash
git pull --recurse-submodules
git submodule update --init --recursive
```

## Licensing

All packages are MIT except `linear-vect` which is BSD-3-Clause (fork with upstream copyright holders).
