
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

# Ghoddess

configList+="Ghoddess/swe_o0_rk2_l4_uniform "
configList+="Ghoddess/swe_o0_rk2_l4_uniform_serial "
configList+="Ghoddess/swe_o1_rk2_l5_bahamas_b4_f82 "

# IO TEST: Non-Uniform
configList+="IOTest/2D_Test_PrintField_NonUniform_NonAA "
configList+="IOTest/2D_Test_PrintField_NonUniform_AA "
configList+="IOTest/3D_Test_PrintField_NonUniform_NonAA "
configList+="IOTest/3D_Test_PrintField_NonUniform_AA "

# IO TEST: Uniform
configList+="IOTest/2D_Scalar_CheckEquality_ReadAfterWrite "
configList+="IOTest/2D_Vector_CheckEquality_ReadAfterWrite "
configList+="IOTest/3D_Matrix_CheckEquality_ReadAfterWrite "
configList+="IOTest/3D_Scalar_CheckEquality_ReadAfterWrite "
configList+="IOTest/3D_Vector_CheckEquality_ReadAfterWrite "
configList+="IOTest/CheckEquality_ReadAfterWrite "

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

