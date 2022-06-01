#!/usr/bin/env python3

import os
import json


####################
# - config class - #
####################


class ConfigFromKnowledge:

    def parse_knowledge(self, knowledge_file_path):
        with open(knowledge_file_path) as knowledge_file:
            for line in knowledge_file:
                # parse imported file
                if line.startswith("import"):
                    # get path of imported file
                    import_path = line.split()[1].replace("\'", "")
                    import_path = import_path.replace("\"", "").strip()
                    if not os.path.isabs(import_path):
                        import_path = os.path.join(os.path.dirname(knowledge_file_path), import_path)

                    # check if file exists
                    if not os.path.isfile(import_path):
                        raise ValueError(f"Knowledge file {import_path} included in {knowledge_file_path} does not exist")

                    # parse imported file
                    self.parse_knowledge(import_path)
                else:
                    self.parse_line_knowledge(line)

    def parse_line_knowledge(self, line):
        line = line.partition('//')[0]  # take everything before comment
        if len(line.strip()) != 0:
            key = line.split('=')[0].strip()
            value = line.split('=')[1].strip()

            if key == "domain_numBlocks":
                self.n_blocks = int(value)
            elif key == "mpi_numThreads":
                self.mpi_num_processes = int(value)
            elif key == "omp_numThreads":
                self.omp_num_threads = int(value)
            elif key == "mpi_enabled":
                self.mpi_enabled = json.loads(value.lower())
            elif key == "omp_enabled":
                self.omp_enabled = json.loads(value.lower())
            elif key == "cuda_enabled":
                self.use_cuda = json.loads(value.lower())

    def parse_line_platform(self, line):
        line = line.partition('//')[0]  # take everything before comment
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

    def __init__(self, problem_name: str, knowledge_file_path: str, platform_file_path: str):

        self.cores_per_cpu = 1
        self.start_2nd_socked_with_0 = True
        self.n_blocks = 1
        self.n_nodes = 1
        self.num_cpus = 1
        self.omp_num_threads = 1
        self.omp_enabled = False
        self.omp_pinning_layout = "compact"
        self.run_local = True
        self.use_cuda = False
        self.mpi_enabled = False
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
        self.parse_knowledge(knowledge_file_path)

        # fetch info from .platform file
        with open(platform_file_path) as platform_file:
            for line in platform_file:
                self.parse_line_platform(line)
