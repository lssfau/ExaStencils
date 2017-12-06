#!/bin/bash

configList=""

# Stokes

configList+="Stokes/1D_FD_Stokes_fromL1 "

configList+="Stokes/2D_FD_Stokes_fromL1 "
configList+="Stokes/2D_FD_Stokes_fromL2 "

configList+="Stokes/2D_FV_Stokes_fromL2 "

configList+="Stokes/3D_FD_Stokes_fromL1 "
configList+="Stokes/3D_FD_Stokes_fromL2 "

configList+="Stokes/3D_FV_Stokes_fromL2 "

configList+="Stokes/2D_FD_Stokes_Lisa_fromL2 "

# Poisson

configList+="Poisson/1D_FD_Poisson_fromL1 "

configList+="Poisson/2D_FD_Poisson_fromL1 "
configList+="Poisson/2D_FD_Poisson_fromL2 "

configList+="Poisson/2D_FV_Poisson_fromL2 "

configList+="Poisson/3D_FD_Poisson_fromL1 "
configList+="Poisson/3D_FD_Poisson_fromL2 "

configList+="Poisson/3D_FV_Poisson_fromL2 "

# OpticalFlow

configList+="OpticalFlow/2D_FD_OptFlow_fromL2 "
configList+="OpticalFlow/2D_FD_OptFlow_fromL4 "
configList+="OpticalFlow/2D_FD_OptFlow_fromL4_Vec "

configList+="OpticalFlow/3D_FD_OptFlow_fromL2 "
configList+="OpticalFlow/3D_FD_OptFlow_fromL4_Vec "

# SWE

configList+="SWE/2D_FV_SWE "

