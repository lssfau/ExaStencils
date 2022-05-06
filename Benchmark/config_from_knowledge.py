import numpy as np
import os
import json


class ConfigFromKnowledge:

    def __init__(self, problem_name: str, knowledge_file_path: str, platform_file_path: str):

        self.cores_per_cpu = 1
        self.start_2nd_socked_with_0 = True
        self.n_blocks = 1
        self.n_nodes = 1
        self.omp_num_threads = 1
        self.omp_pinning_layout = "compact"
        self.run_local = True
        self.use_cuda = False
        self.mpi_pinning_domain = "socket"
        self.mpi_num_processes = 1
        self.simd_instructionSet = ""
        self.cpu_name = ""
        self.gpu_name = ""
        self.host_name = ""
        self.problem_name = problem_name

        assert (os.path.isfile(knowledge_file_path) and os.path.isfile(platform_file_path))

        self.host_name = os.path.splitext(os.path.basename(platform_file_path))[0]

        # fetch info from .knowledge file
        with open(knowledge_file_path) as knowledge_file:
            for line in knowledge_file:
                if len(line.strip()) != 0:
                    key = line.split('=')[0].strip()
                    value = line.split('=')[1].strip()

                    print(line)

                    if key == "domain_numBlocks":
                        self.n_blocks = int(value)
                    elif key == "mpi_numThreads":
                        self.mpi_num_processes = int(value)
                    elif key == "omp_numThreads":
                        self.omp_num_threads = int(value)
                    elif key == "cuda_enabled":
                        self.use_cuda = json.loads(value.lower())

        # fetch info from .platform file
        with open(platform_file_path) as knowledge_file:
            for line in knowledge_file:
                if len(line.strip()) != 0:
                    key = line.split('=')[0].strip()
                    value = line.split('=')[1].strip()

                    if key == "hw_numNodes":
                        self.n_nodes = int(value)
                    elif key == "simd_instructionSet":
                        self.simd_instructionSet = value
                    elif key == "hw_cpu_name":
                        self.cpu_name = value
                    elif key == "hw_gpu_name":
                        self.gpu_name = value
                    elif key == "hw_cpu_numCoresPerCPU":
                        self.cores_per_cpu = int(value)
                    elif key == "hw_cpu_numCPUs":
                        self.num_cpus = int(value)
