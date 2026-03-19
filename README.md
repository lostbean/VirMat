# VirMat: Virtual Microstructure Generator

**VirMat** is a high-performance 2D and 3D virtual microstructure generator written in Haskell, designed for metallurgical and materials science research. It creates synthetic polycrystalline microstructures with user-defined grain size distributions and crystallographic textures, producing output suitable for visualization in [ParaView](https://www.paraview.org/) and analysis with EBSD software.

## Core Features

- **2D and 3D generation** -- produce microstructures in either dimensionality from a single CLI.
- **Statistical grain size control** -- specify Log-Normal, Normal, Uniform, or custom grain diameter distributions; combine multiple distributions for multi-modal populations.
- **Sphere/circle packing** -- optional iterative packing (Verlet integration with damping) to obtain realistic, non-overlapping grain seed placement before tessellation.
- **Voronoi tessellation** -- Delaunay triangulation (DeWall algorithm) followed by dual Voronoi construction via `MicroGraph`.
- **Subdivision surfaces** -- Voronoi facets are converted to Loop subdivision surfaces (`SubZero`) so grain boundaries can be smoothly refined to arbitrary resolution.
- **Crystallographic texture** -- orientations sampled from a Bingham distribution are assigned per grain; IPF coloring is rendered into VTK attributes.
- **Phase transformation** -- a parent microstructure can be overlaid with a finer product microstructure and each product grain inherits the parent grain identity.
- **Export formats**
  - VTK Unstructured Grid (`.vtu`) for 3D/2D visualization in ParaView.
  - ANG (`.ang`) for 2D EBSD-like data analysis (rasterized from the subdivision mesh).

## Architecture Overview

### Generation Pipeline

```
JobRequest (CLI)
  |
  v
Grain Size Sampling          -- Core.Sampling + Distributions.GrainSize
  |  (inverse-CDF sampling from composed multi-distributions)
  v
Weighted Point Cloud         -- GrainDistributionGenerator
  |  (circles/spheres with diameter-derived radii)
  v
[Optional] Sphere Packing   -- Core.Packer  (Verlet integration, N iterations)
  |
  v
Delaunay Triangulation       -- DeUni (DeWall algorithm, 2D or 3D)
  |
  v
Voronoi Micro-graph          -- Core.VoronoiMicro  (dual of Delaunay -> MicroGraph)
  |
  v
FlexMicro (Subdivision)      -- Core.FlexMicro  (Loop subdivision surfaces via SubZero)
  |
  +---> Texture Assignment   -- Distributions.Texture.ODFSampling (Bingham sampling)
  |
  +---> Morphology Query     -- Distributions.GrainSize.GrainQuery (area, volume, neighbors)
  |
  +---> VTK Rendering        -- FlexMicro.renderFlexMicro -> .vtu files
  |
  +---> ANG Rasterization    -- IO.Export.ANG.RasterEngine -> .ang files
  |
  +---> Phase Transformation -- PhaseTrans (parent/product overlay)
```

### Data Flow Types

| Type | Description |
|------|-------------|
| `JobRequest` | Parsed CLI parameters: dimension, grain count/box size, distribution, seed |
| `DistributedPoints v` | Bounding box + weighted point cloud (`SetPoint v`) |
| `Simulation v` | Full state: box, points, Delaunay triangulation, Voronoi micro-graph |
| `FlexMicro v a` | Subdivision-surface microstructure with per-grain property `a` |
| `GrainMorph v` | Computed morphological properties (center, length, area, volume, neighbor count) |

## Packages

VirMat is organized as a multi-package Haskell project. The root package orchestrates the generation pipeline, while specialized functionality is pulled from several core libraries via `cabal.project` (previously git submodules):

| Package | Description |
|---------|-------------|
| [DeUni](https://github.com/lostbean/DeUni) | 3D Convex Hull and Delaunay triangulation using the Marriage-Before-Conquer (MBC) / DeWall algorithm |
| [hammer](https://github.com/lostbean/hammer) | Metallurgical utilities: `MicroGraph` topology, grain finding, sparse matrices, and VTK generation |
| [sledge](https://github.com/lostbean/sledge) | Crystallographic orientations, symmetries, Bingham distributions, IPF coloring, and EBSD file formats (ANG, CTF) |
| [linear-vect](https://github.com/lostbean/linear-vect) | Low-dimensional linear algebra: `Vec2`, `Vec3`, `Vec4`, matrices, quaternions |
| [queryforest](https://github.com/lostbean/queryforest) | Spatial indexing and nearest-neighbor search (KD-trees, VP-trees) |
| [SubZero](https://github.com/lostbean/SubZero) | Subdivision surfaces for 1D (lines/curves) and 2D (triangular meshes, Loop scheme) |
| [VTK](https://github.com/lostbean/VTK) | Library for generating VTK XML files (`.vtu`, `.vti`, etc.) |
| [mcl](https://github.com/lostbean/mcl) | Markov Cluster Algorithm (MCL) for graph clustering |

## Key Modules and Source Files

### Entry Point

| File | Module | Role |
|------|--------|------|
| `src/Main.hs` | `Main` | CLI entry point; dispatches to `go2D` or `go3D` based on `--2d`/`--3d` flag |

### Core Library (`VirMat.*`)

| File | Module | Role |
|------|--------|------|
| `src/VirMat/Types.hs` | `VirMat.Types` | `Simulation` record: box, point set, triangulation, grain set |
| `src/VirMat/Run2D.hs` | `VirMat.Run2D` | 2D pipeline: sample points, optional packing, Delaunay, Voronoi |
| `src/VirMat/Run3D.hs` | `VirMat.Run3D` | 3D pipeline: same stages for 3D |
| `src/VirMat/Core/Sampling.hs` | `VirMat.Core.Sampling` | Statistical distributions (LogNormal, Normal, Uniform, Custom), inverse-CDF sampling |
| `src/VirMat/Core/Packer.hs` | `VirMat.Core.Packer` | Iterative sphere/circle packing via Verlet integration with force model and damping |
| `src/VirMat/Core/VoronoiMicro.hs` | `VirMat.Core.VoronoiMicro` | Converts Delaunay simplices to Voronoi `MicroGraph` (dual construction) |
| `src/VirMat/Core/FlexMicro.hs` | `VirMat.Core.FlexMicro` | Converts Voronoi polygons/polyhedra to Loop subdivision surfaces; VTK rendering |

### Distributions

| File | Module | Role |
|------|--------|------|
| `src/VirMat/Distributions/GrainSize/GrainDistributionGenerator.hs` | `...GrainDistributionGenerator` | Generates weighted point clouds by grain count or bounding box volume |
| `src/VirMat/Distributions/GrainSize/GrainQuery.hs` | `...GrainQuery` | Computes per-grain morphological properties from subdivision meshes |
| `src/VirMat/Distributions/Texture/ODFSampling.hs` | `...ODFSampling` | Assigns Bingham-sampled crystallographic orientations to grains; IPF RGB coloring |

### I/O

| File | Module | Role |
|------|--------|------|
| `src/VirMat/IO/Import/CommandLine.hs` | `...CommandLine` | CLI parser (optparse-applicative) |
| `src/VirMat/IO/Import/Types.hs` | `...Import.Types` | `JobRequest`, `Dimension`, `StructureSize`, `DistributionType`, `Output` |
| `src/VirMat/IO/Export/ANG/RasterEngine.hs` | `...ANG.RasterEngine` | Triangle rasterization engine that converts `FlexMicro` grains to an ANG grid |
| `src/VirMat/IO/Export/VTK/VTKODFRender.hs` | `...VTK.VTKODFRender` | Renders a discrete ODF to a VTK ImageData (`.vti`) file |

### Other

| File | Module | Role |
|------|--------|------|
| `src/VirMat/PhaseTrans.hs` | `VirMat.PhaseTrans` | Phase transformation simulation: parent/product microstructure overlay |

## Building

### Prerequisites

- [Cabal](https://www.haskell.org/cabal/) or [Stack](https://docs.haskellstack.org/en/stable/)
- GHC 9.10.x (recommended)

### With Cabal

```bash
git clone <repo-url>
cd VirMat

# Build the entire project and dependencies
cabal build

# Run the executable
cabal run virmatgen -- --help
```

### With Stack

```bash
git clone <repo-url>
cd VirMat

# Build the entire project
stack build

# Run the executable
stack exec virmatgen -- --help
```

### With Nix (development shell)

The project provides a Nix flake for a reproducible development environment:

```bash
# Enter the dev shell (provides GHC, Stack, Cabal, HLS, zlib, clang, treefmt)
nix develop

# Then build with Cabal or Stack as usual
cabal build
```

## Testing

VirMat includes a comprehensive test suite using `hspec` and `QuickCheck`.

```bash
# Run all tests
cabal test

# Run tests with coverage report
cabal test --enable-coverage
```

The test suite covers:
- **Core.Packer**: Force models and bounding box constraints.
- **Core.Sampling**: Statistical distribution area, mean, and composition.
- **Distributions.GrainSize**: Bounding box geometry and volumetric consistency.
- **GrainQuery**: Geometric primitives (triangle area, tetrahedron volume).

## CLI Usage

The primary executable is `virmatgen`. It generates microstructures based on command-line parameters.

```
virmatgen - Virtual microstructure generator in 3D/2D.

Usage: virmatgen [--3d | --2d]
                 [--n2d INT | --n2d (DOUBLE,DOUBLE)]
                 [--packed-n INT | --packed | --random]
                 [--lnorm (k,mu,mode,o) | --norm (k,mu,s) | --uniform (k,mu,s)] ...
                 [--seed INT]
                 -d FILEPATH -s STR
                 [--showvoronoi] [--showbox] [--showhull]
                 [--showpoints] [--showsimplex] [--showforces]
```

### Key Options

| Flag | Description | Default |
|------|-------------|---------|
| `--3d` / `--2d` | Dimensionality of the microstructure | `--3d` |
| `--n2d INT` | Number of grains | `500` |
| `--packed` | Enable sphere packing (60 iterations) | enabled |
| `--packed-n INT` | Sphere packing with custom iteration count | -- |
| `--random` | Random (non-packed) grain placement | -- |
| `--lnorm (k,mu,mode,o)` | Log-Normal grain size distribution | -- |
| `--norm (k,mu,s)` | Normal grain size distribution | -- |
| `--uniform (k,mu,s)` | Uniform grain size distribution | -- |
| `--seed INT` | Random seed for reproducibility | system random |
| `-d FILEPATH` | Output directory | required |
| `-s STR` | Sample name | required |

Distribution parameters: `k` = scaling factor, `mu` = average/mean, `s` = variance, `mode` = distribution mode, `o` = offset. Multiple `--lnorm`, `--norm`, and `--uniform` flags can be combined for multi-modal distributions.

### Output Files

| File | Format | Content |
|------|--------|---------|
| `virmat-3d.vtu` | VTK Unstructured Grid | 3D microstructure with grain ID, volume, area, neighbor count, and IPF-ND coloring |
| `virmat-2d.vtu` | VTK Unstructured Grid | 2D microstructure with grain ID, area, boundary length, and neighbor count |
| `virmat-2d.ang` | ANG (EBSD) | Rasterized 2D orientation map for EBSD analysis tools (OIM, MTEX, etc.) |

### Example

```bash
# Generate a 3D microstructure with 200 grains, log-normal size distribution, packed
stack exec virmatgen -- --3d --n2d 200 --packed \
    --lnorm '(1.0, 5.0, 3.0, 0.0)' \
    --seed 42 \
    -d ./output -s mysample

# Generate a 2D microstructure with normal distribution, 100 grains
stack exec virmatgen -- --2d --n2d 100 --packed \
    --norm '(1.0, 5.0, 1.0)' \
    --seed 42 \
    -d ./output -s mysample2d
```

## Dependencies

### Haskell (from Hackage)

`base`, `containers`, `mersenne-random-pure64`, `mtl`, `optparse-applicative`, `random`, `random-fu`, `transformers`, `unordered-containers`, `vector`

### Internal Packages

These are managed via `cabal.project` and `stack.yaml` as source-repository-packages:
`DeUni`, `hammer`, `sledge`, `linear-vect`, `SubZero`, `queryforest`, `VTK`, `mcl`

## Author

Edgar Gomes de Araujo (<talktoedgar@gmail.com>)

## License

MIT -- see [LICENSE](./LICENSE).
