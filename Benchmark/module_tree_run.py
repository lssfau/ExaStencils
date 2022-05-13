#!/usr/bin/env python3

import subprocess
import os
from run_benchmark import check_err


# --- load modules for compiling and running the generated code --- #

@check_err
def load_mpi_module():
    os.system("module load openmpi/4.1.1-gcc")
    return subprocess.run(["mpirun", "--version"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)


@check_err
def load_likwid_module():
    os.system("module load likwid/5.2.1")
    return subprocess.run(["likwid-features", "--version"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)


@check_err
def check_nvcc():
    return subprocess.run(["nvcc", "--version"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)


@check_err
def check_nvidia_smi():
    return subprocess.run(["nvidia-smi"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)


@check_err
def load_cuda_module():
    os.system("module load cuda/11.4")
    check_nvcc()
    check_nvidia_smi()
    return subprocess.run(["updatedb"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
