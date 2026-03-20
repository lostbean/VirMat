# VirMat

Virtual microstructure generator for 2D and 3D polycrystalline materials.

## What is this?

VirMat generates synthetic polycrystalline microstructures -- the kind of grain structures you see when you look at a metal under a microscope. Given a statistical description of grain sizes (e.g., "log-normal distribution with mean 5 micrometers"), VirMat produces a realistic 3D (or 2D) arrangement of grains with smooth boundaries, crystallographic orientations, and morphological properties.

The output can be visualized in [ParaView](https://www.paraview.org/) (VTK format) or analyzed with EBSD software like OIM or MTEX (ANG format).

## Why generate virtual microstructures?

Real microstructure characterization is expensive and limited: EBSD scans are 2D slices, serial sectioning is destructive, and 3D EBSD is still slow and costly. Virtual microstructures provide:

- **Input for simulations** -- finite element models, crystal plasticity simulations, and phase-field models need realistic starting microstructures
- **Statistical validation** -- compare simulated grain size distributions, neighbor counts, and morphological properties against experimental data
- **Parametric studies** -- vary grain size, texture, or packing without needing new experiments
- **3D structure from 2D statistics** -- generate full 3D microstructures from grain size distributions measured on 2D cross-sections

## The generation pipeline

### 1. Grain size sampling

Grain sizes are drawn from user-specified statistical distributions. Supported distributions:

- **Log-normal** -- the most common grain size distribution in real materials
- **Normal** -- symmetric distribution
- **Uniform** -- constant probability within bounds
- **Custom** -- arbitrary histogram
- **Composed** -- combine multiple distributions for multi-modal populations (e.g., bimodal grain sizes after partial recrystallization)

Sampling uses inverse-CDF (cumulative distribution function) interpolation.

### 2. Sphere packing

Grain seed points are placed as spheres/circles with diameters corresponding to the sampled grain sizes. An optional iterative packing step uses Verlet integration with a force model (quadratic repulsion for overlaps, attraction for free space) to arrange grains into a realistic, non-overlapping configuration.

### 3. Voronoi tessellation

The packed seed points are triangulated using Delaunay triangulation (via `DeUni`), then the geometric dual -- the Voronoi tessellation -- is extracted. Each Voronoi cell becomes a grain. The Voronoi dual is stored as a microstructure graph (via `hammer`) encoding grains, grain boundaries (faces), triple lines (edges), and quadruple points (vertices).

### 4. Subdivision smoothing

The flat-faceted Voronoi polyhedra are refined using Loop subdivision surfaces (via `SubZero`). A few levels of subdivision produce smooth, realistic grain boundaries suitable for visualization and analysis.

### 5. Texture assignment

Each grain is assigned a crystallographic orientation sampled from a Bingham distribution (via `sledge`). Inverse Pole Figure (IPF) colors are computed and attached as VTK attributes for visualization.

### 6. Export

- **VTK** (`.vtu`) -- 3D and 2D microstructures with grain ID, volume, area, neighbor count, and IPF coloring
- **ANG** (`.ang`) -- 2D orientation map rasterized from the subdivision mesh, compatible with EBSD analysis tools

## Ecosystem packages

VirMat orchestrates several specialized libraries:

| Package | Role |
|---------|------|
| [DeUni](https://github.com/lostbean/DeUni) | Delaunay triangulation (Marriage Before Conquer algorithm) |
| [hammer](https://github.com/lostbean/hammer) | Microstructure graph, grain finding, VTK export |
| [sledge](https://github.com/lostbean/sledge) | Crystallographic orientations, Bingham distributions, EBSD I/O |
| [linear-vect](https://github.com/lostbean/linear-vect) | Low-dimensional linear algebra (vectors, matrices) |
| [queryforest](https://github.com/lostbean/queryforest) | Spatial indexing (VP-trees, KD-trees) |
| [SubZero](https://github.com/lostbean/SubZero) | Subdivision surfaces (Loop scheme) |
| [VTK](https://github.com/lostbean/VTK) | VTK XML file generation |
| [mcl](https://github.com/lostbean/mcl) | Markov Cluster Algorithm for graph clustering |

## Example

```bash
# 3D microstructure, 200 grains, log-normal distribution, packed
virmatgen --3d --n2d 200 --packed \
    --lnorm '(1.0, 5.0, 3.0, 0.0)' \
    --seed 42 \
    -d ./output -s mysample

# 2D microstructure, 100 grains, normal distribution
virmatgen --2d --n2d 100 --packed \
    --norm '(1.0, 5.0, 1.0)' \
    --seed 42 \
    -d ./output -s mysample2d
```

### Key CLI options

| Flag | Description | Default |
|------|-------------|---------|
| `--3d` / `--2d` | Dimensionality | 3D |
| `--n2d INT` | Number of grains | 500 |
| `--packed` / `--packed-n INT` | Enable packing (with optional iteration count) | 60 iterations |
| `--random` | Random placement (no packing) | -- |
| `--lnorm (k,mu,mode,o)` | Log-normal distribution | -- |
| `--norm (k,mu,s)` | Normal distribution | -- |
| `--uniform (k,mu,s)` | Uniform distribution | -- |
| `--seed INT` | Random seed | system random |
| `-d FILEPATH` | Output directory | required |
| `-s STR` | Sample name | required |

Multiple distribution flags can be combined for multi-modal grain size populations.

## How to build

```bash
# With Nix (recommended)
nix develop
cabal build

# With Stack
stack build

# Run the executable
cabal run virmatgen -- --help

# Run tests
cabal test
```

## License

MIT -- see [LICENSE](./LICENSE).
