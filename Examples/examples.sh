
#!/bin/bash

configList=""

# BiHarmonic

#configList+="BiHarmonic/2D_FD_BiHarmonic_fromL2 "

# Stokes

#configList+="Stokes/1D_FD_Stokes_fromL1 "

#configList+="Stokes/2D_FD_Stokes_fromL1 "
#configList+="Stokes/2D_FD_Stokes_fromL2 "
#configList+="Stokes/2D_FD_Stokes_fromL4 "

#configList+="Stokes/2D_FV_Stokes_fromL2 "
#configList+="Stokes/2D_FV_Stokes_fromL4 "

#configList+="Stokes/3D_FD_Stokes_fromL1 "
#configList+="Stokes/3D_FD_Stokes_fromL2 "
#configList+="Stokes/3D_FD_Stokes_fromL4 "

#configList+="Stokes/3D_FV_Stokes_fromL2 "
#configList+="Stokes/3D_FV_Stokes_fromL4 "

#configList+="Stokes/2D_FD_Stokes_Lisa_fromL2 "

# Poisson

#configList+="Poisson/1D_FD_Poisson_fromL1 "

#configList+="Poisson/2D_FD_Poisson_fromL1 "
#configList+="Poisson/2D_FD_Poisson_fromL2 "
#configList+="Poisson/2D_FD_Poisson_fromL3 "
#configList+="Poisson/2D_FD_Poisson_fromL4 "

#configList+="Poisson/2D_FV_Poisson_fromL2 "
#configList+="Poisson/2D_FV_Poisson_fromL4 "

#configList+="Poisson/3D_FD_Poisson_fromL1 "
#configList+="Poisson/3D_FD_Poisson_fromL2 "
#configList+="Poisson/3D_FD_Poisson_fromL4 "

#configList+="Poisson/3D_FV_Poisson_fromL2 "
#configList+="Poisson/3D_FV_Poisson_fromL4 "

# OpticalFlow

#configList+="OpticalFlow/2D_FD_OptFlow_fromL2 "
#configList+="OpticalFlow/2D_FD_OptFlow_fromL4 "
#configList+="OpticalFlow/2D_FD_OptFlow_fromL4_Vec "

#configList+="OpticalFlow/3D_FD_OptFlow_fromL2 "
#configList+="OpticalFlow/3D_FD_OptFlow_fromL4_Vec "

# SWE

#configList+="SWE/2D_FV_SWE "

# IO: Non-Uniform field visualization
configList+="IO/2D_PrintField_NonUniform_NonAA "
configList+="IO/2D_PrintField_NonUniform_AA "
configList+="IO/3D_PrintField_NonUniform_NonAA "
configList+="IO/3D_PrintField_NonUniform_AA "

# NavierStokes

#configList+="NavierStokes/2D_FV_NavierStokes_Picard "
#configList+="NavierStokes/2D_FV_NavierStokes_Newton "
configList+="NavierStokes/2D_FV_NavierStokes_localPicard "
#configList+="NavierStokes/2D_FV_NavierStokes_localNewton "
#
#configList+="NavierStokes/3D_FV_NavierStokes_Picard "
#configList+="NavierStokes/3D_FV_NavierStokes_Newton "
configList+="NavierStokes/3D_FV_NavierStokes_localPicard "
#configList+="NavierStokes/3D_FV_NavierStokes_localNewton "
#
#configList+="NavierStokes/2D_FV_NonNewtonian_Picard "
#configList+="NavierStokes/2D_FV_NonNewtonian_Newton "
#
configList+="NavierStokes/3D_FV_NonNewtonian_Picard "
#configList+="NavierStokes/3D_FV_NonNewtonian_Newton "

# LinearElasticity

#configList+="LinearElasticity/2D_FD_LinearElasticity_fromL2 "

# Helmholtz
configList+="Helmholtz/2D_FD_Helmholtz_fromL3 "

