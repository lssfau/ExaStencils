#!/usr/bin/env python3

import numpy as np
from config_from_knowledge import *


#####################
# - pin functions - #
#####################


def get_omp_exp(config: ConfigFromKnowledge, nthreads: int, cpu_id_start: int):
    if cpu_id_start < config.cores_per_cpu:
        socket_id = 0
    else:
        socket_id = 1
    cpu_id_end = cpu_id_start + nthreads - 1
    if cpu_id_end > config.cores_per_cpu - 1 and socket_id == 0:
        pinning_expression = f"S{socket_id}:{cpu_id_start}-{config.cores_per_cpu - 1}"
        socket_id = 1
        cpu_id_start = config.cores_per_cpu
        pinning_expression = pinning_expression + f"@S{socket_id}:{cpu_id_start}-{cpu_id_end}"
    else:
        if config.start_2nd_socked_with_0:
            pinning_expression = f"S{socket_id}:{0}-{nthreads - 1}"
        else:
            pinning_expression = f"S{socket_id}:{cpu_id_start}-{cpu_id_end}"
    return pinning_expression


def get_omp_pinning(config: ConfigFromKnowledge, nthreads_per_task: int, ntasks: int, cpu_id_start: int):
    task_id = 0
    pinning = ""
    while task_id < ntasks:
        if task_id == 0:
            pinning = get_omp_exp(config, nthreads_per_task, cpu_id_start)
        else:
            pinning = pinning + f"_{get_omp_exp(config, nthreads_per_task, cpu_id_start)}"
        cpu_id_start += nthreads_per_task
        task_id += 1
    return pinning


def likwid_pin(config: ConfigFromKnowledge, use_likwid_perfctr: bool):
    ntasks_per_node = config.n_blocks // config.n_nodes
    omp_pinning = "S0:0"
    if config.omp_num_threads > 1:
        if config.omp_pinning_layout == "distribute":
            pin_S0 = get_omp_exp(config, int(np.ceil(config.omp_num_threads / 2)), 0)
            pin_S1 = get_omp_exp(config, int(np.floor(config.omp_num_threads / 2)), config.cores_per_cpu)
            omp_pinning = f"{pin_S0}@{pin_S1}"
        elif config.omp_pinning_layout == "compact":
            omp_pinning = get_omp_exp(config, config.omp_num_threads, 0)

    bin = ["./exastencils"]
    likwid_perfctr_args = ["-g", "Exa", "-m"] if use_likwid_perfctr else []

    # no MPI, pure OpenMP
    if config.n_blocks == 1:
        likwid_bin = ["likwid-perfctr"] + likwid_perfctr_args if use_likwid_perfctr else ["likwid-pin"]
        if config.run_local:
            if not config.use_cuda:
                return likwid_bin + ["-c", f"{omp_pinning}"] + bin
            else:
                return likwid_bin + ["-c", f"{omp_pinning}", "-s", "0x3"] + bin
        else:
            if not config.use_cuda:
                return ["srun"] + likwid_bin + ["-c", f"{omp_pinning}"] + bin
            else:
                return ["srun"] + likwid_bin + ["-c", f"{omp_pinning}", "-s", "0x3"] + bin
    # pure MPI, no OpenMP
    elif config.n_blocks > 1 and (config.omp_num_threads == 1 or config.omp_enabled is False) and config.use_cuda is False:
        return ["env", "OMPI_MCA_hwloc_base_binding_policy=none", "likwid-mpirun", "-d", "-np", "-s", "0x3"] + \
               likwid_perfctr_args + [f"{config.mpi_num_processes}"] + bin
    # hybrid
    elif config.n_blocks > 1:
        # omp pinning
        if "node" == config.mpi_pinning_domain:
            omp_pinning = get_omp_pinning(config, config.omp_num_threads, ntasks_per_node, 0)
        elif "socket" == config.mpi_pinning_domain:
            pin_S0 = get_omp_pinning(config, config.omp_num_threads, int(np.ceil(ntasks_per_node / 2)), 0)
            pin_S1 = get_omp_pinning(config, config.omp_num_threads, int(np.floor(ntasks_per_node / 2)),
                                     config.cores_per_cpu)
            omp_pinning = f"{pin_S0}_{pin_S1}"
        ntasks_per_node_ = f"N:{ntasks_per_node}"
        if not config.use_cuda:
            return ["env", f"OMP_NUM_THREADS={config.omp_num_threads}", "env", "OMPI_MCA_hwloc_base_binding_policy=none",
                    "likwid-mpirun", "-d", "-np", f"{config.mpi_num_processes}", "-nperdomain",
                    f"{ntasks_per_node_}", "-pin", f"{omp_pinning}", f"{config.omp_num_threads}"] + \
                    likwid_perfctr_args + ["-s", "0x3"] + bin
        else:
            return ["env", f"OMP_NUM_THREADS={config.omp_num_threads}", "env", "OMPI_MCA_hwloc_base_binding_policy=none",
                    "likwid-mpirun", "-d", "-np", f"{config.mpi_num_processes}", "-nperdomain",
                    f"{ntasks_per_node_}", "-pin", f"{omp_pinning}", f"{config.omp_num_threads}"] + \
                    likwid_perfctr_args + ["-s", "0x7"] + bin
