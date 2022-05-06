import numpy as np


def get_omp_exp(cores_per_socket: int, start_2nd_socked_with_0: bool, nthreads: int, cpu_id_start: int):
    if cpu_id_start < cores_per_socket:
        socket_id = 0
    else:
        socket_id = 1
    cpu_id_end = cpu_id_start + nthreads - 1
    if cpu_id_end > cores_per_socket - 1 and socket_id == 0:
        pinning_expression = f"S{socket_id}:{cpu_id_start}-{cores_per_socket - 1}"
        socket_id = 1
        cpu_id_start = cores_per_socket
        pinning_expression = pinning_expression + f"@S{socket_id}:{cpu_id_start}-{cpu_id_end}"
    else:
        if start_2nd_socked_with_0:
            pinning_expression = f"S{socket_id}:{0}-{nthreads - 1}"
        else:
            pinning_expression = f"S{socket_id}:{cpu_id_start}-{cpu_id_end}"
    return pinning_expression


def get_omp_pinning(cores_per_socket: int, start_2nd_socked_with_0: bool, nthreads_per_task: int, ntasks: int,
                    cpu_id_start: int):
    task_id = 0
    pinning = ""
    while task_id < ntasks:
        if task_id == 0:
            pinning = get_omp_exp(cores_per_socket, start_2nd_socked_with_0, nthreads_per_task, cpu_id_start)
        else:
            pinning = pinning + f"_{get_omp_exp(cores_per_socket, start_2nd_socked_with_0, nthreads_per_task, cpu_id_start)}"
        cpu_id_start += nthreads_per_task
        task_id += 1
    return pinning


def pin_executable(cores_per_socket: int, start_2nd_socked_with_0: bool,
                   omp_pinning_layout: str, mpi_pinning_domain: str, n_blocks: int, n_nodes: int,
                   omp_num_threads: int, mpi_num_processes: int, use_cuda: bool):
    ntasks_per_node = n_blocks // n_nodes
    omp_pinning = "S0:0"
    if omp_num_threads > 1:
        if omp_pinning_layout == "distribute":
            pin_S0 = get_omp_exp(cores_per_socket, start_2nd_socked_with_0, int(np.ceil(omp_num_threads / 2)), 0)
            pin_S1 = get_omp_exp(cores_per_socket, start_2nd_socked_with_0, int(np.floor(omp_num_threads / 2)),
                                 cores_per_socket)
            omp_pinning = f"{pin_S0}@{pin_S1}"
        elif omp_pinning_layout == "compact":
            omp_pinning = get_omp_exp(cores_per_socket, start_2nd_socked_with_0, omp_num_threads, 0)

    # no MPI, pure OpenMP
    if n_blocks == 1:
        if not use_cuda:
            return "likwid-pin", "-c", f"{omp_pinning}", "./exastencils"
        else:
            return "likwid-pin", "-c", f"{omp_pinning}", "-s", "0x3", "./exastencils"
    # with MPI
    else:
        # omp pinning
        if "node" == mpi_pinning_domain:
            omp_pinning = get_omp_pinning(cores_per_socket, start_2nd_socked_with_0, omp_num_threads, ntasks_per_node,
                                          0)
        elif "socket" == mpi_pinning_domain:
            pin_S0 = get_omp_pinning(cores_per_socket, start_2nd_socked_with_0, omp_num_threads,
                                     int(np.ceil(ntasks_per_node / 2)), 0)
            pin_S1 = get_omp_pinning(cores_per_socket, start_2nd_socked_with_0, omp_num_threads,
                                     int(np.floor(ntasks_per_node / 2)),
                                     cores_per_socket)
            omp_pinning = f"{pin_S0}_{pin_S1}"

        ntasks_per_node_ = f"N:{ntasks_per_node}"
        if not use_cuda:
            return "env", f"OMP_NUM_THREADS={omp_num_threads}", "env", "OMPI_MCA_hwloc_base_binding_policy=none", "likwid-mpirun", "-d", "-np", f"{mpi_num_processes}", "-nperdomain", f"{ntasks_per_node_}", "-pin", f"{omp_pinning}", f"{omp_num_threads}", "-s", "0x3", "./exastencils"
        else:
            return "env", f"OMP_NUM_THREADS={omp_num_threads}", "env", "OMPI_MCA_hwloc_base_binding_policy=none", "likwid-mpirun", "-d", "-np", f"{mpi_num_processes}", "-nperdomain", f"{ntasks_per_node_}", "-pin", f"{omp_pinning}", f"{omp_num_threads}", "-s", "0x7", "./exastencils"
