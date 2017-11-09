#!/bin/bash

configList=""

# Stokes

configList+="Stokes/2D_FD_Stokes "
configList+="Stokes/2D_FV_Stokes "

# Poisson

configList+="Poisson/2D_FD_Poisson_usingL1 "
configList+="Poisson/2D_FD_Poisson_usingL2 "

configList+="Poisson/2D_FV_Poisson "
configList+="Poisson/3D_FD_Poisson "
configList+="Poisson/3D_FV_Poisson "

# OpticalFlow

configList+="OpticalFlow/2D_FD_OptFlow "
configList+="OpticalFlow/3D_FD_OptFlow "
configList+="OpticalFlow/2D_FD_OptFlow_onlyL4 "
configList+="OpticalFlow/2D_FD_OptFlow_onlyL4_Vec "
configList+="OpticalFlow/3D_FD_OptFlow_onlyL4_Vec "

# SWE

configList+="SWE/2D_FV_SWE "
