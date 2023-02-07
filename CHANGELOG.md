# Changelog

## v1.0: Oct 17, 2019

Initial release of ExaStencils

## v1.1: Nov 16, 2021

- Overhaul matrix and vector datatypes
- New slot access signature
- Features for GHODDESS
  - Logical partition of quad cells into lower/upper triangle
  - Modifications to domain initialization from file
  - Flexible field selection for vtk output
- Local solve function for systems of equations
- Extended complex datatypes

## v1.2: Feb 7, 2023

- CUDA: Support for managed/pinned memory and zero copy
- Vectorization: Add blending mechanism for masked updates. Support NEON intrinsics
- OpenMP: Provide knowledge flags for OMP schedule clauses
- Allow reductions for higher-order datatypes
- Extend (automatic) performance evaluation capabilities
- Visualization: Implement post-processing routines for XDMF and ExodusII file format. Enable interactive visualization and computational steering using VisIt libsim
- I/O: Add support for (parallel) I/O using MPI-I/O, HDF5, SIONlib and (P)netCDF
- Minor fixes for matrix datatypes

## v2.0: TBA
