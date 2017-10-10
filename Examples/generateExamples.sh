#!/bin/bash

platform=lib/linux.platform

# Stokes

java -cp ../Compiler/Compiler.jar Main Stokes/2D_FD_Stokes.settings Stokes/2D_FD_Stokes.knowledge $platform
java -cp ../Compiler/Compiler.jar Main Stokes/2D_FV_Stokes.settings Stokes/2D_FV_Stokes.knowledge $platform

# Poisson

java -cp ../Compiler/Compiler.jar Main Poisson/2D_FD_Poisson.settings Poisson/2D_FD_Poisson.knowledge $platform
java -cp ../Compiler/Compiler.jar Main Poisson/2D_FV_Poisson.settings Poisson/2D_FV_Poisson.knowledge $platform
java -cp ../Compiler/Compiler.jar Main Poisson/3D_FD_Poisson.settings Poisson/3D_FD_Poisson.knowledge $platform
java -cp ../Compiler/Compiler.jar Main Poisson/3D_FV_Poisson.settings Poisson/3D_FV_Poisson.knowledge $platform

# OpticalFlow

java -cp ../Compiler/Compiler.jar Main OpticalFlow/2D_FD_OptFlow.settings OpticalFlow/2D_FD_OptFlow.knowledge $platform
java -cp ../Compiler/Compiler.jar Main OpticalFlow/3D_FD_OptFlow.settings OpticalFlow/3D_FD_OptFlow.knowledge $platform

java -cp ../Compiler/Compiler.jar Main OpticalFlow/2D_FD_OptFlow_onlyL4.settings OpticalFlow/2D_FD_OptFlow_onlyL4.knowledge $platform

java -cp ../Compiler/Compiler.jar Main OpticalFlow/2D_FD_OptFlow_onlyL4_Vec.settings OpticalFlow/2D_FD_OptFlow_onlyL4_Vec.knowledge $platform
java -cp ../Compiler/Compiler.jar Main OpticalFlow/3D_FD_OptFlow_onlyL4_Vec.settings OpticalFlow/3D_FD_OptFlow_onlyL4_Vec.knowledge $platform
