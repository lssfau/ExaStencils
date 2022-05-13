#!/usr/bin/env python3

import subprocess
from run_benchmark import check_err


# --- load modules for compiling and running the generated code --- #

@check_err
def load_mpi_module():
    subprocess.run(["module", "load", "openmpi/4.1.1-gcc"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    return subprocess.run(["mpirun", "--version"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)


@check_err
def load_likwid_module():
    subprocess.run(["module", "load", "likwid/5.2.1"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    return subprocess.run(["likwid-features", "--version"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)


@check_err
def load_cuda_module():
    subprocess.run(["module", "load", "cuda/11.4"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    result = subprocess.run(["nvcc", "--version"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    if not result.returncode == 0:
        print(result.stderr.decode('utf-8'))
        return result.returncode
    result = subprocess.run(["nvidia-smi"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    subprocess.run(["updatedb"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    return result
