#!/usr/bin/env python3

import subprocess
import argparse
from likwid_pinning import *
from run_benchmark import check_err
from generation_helpers import *


@check_err
def slurm_alloc(config: ConfigFromKnowledge):
    assert config.n_nodes == 1, "Only single-node jobs are currently allowed on the testcluster"

    result = subprocess.run(["salloc", f"--nodes={config.n_nodes}", f"--nodelist={config.host_name}", "--export=NONE"],
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    #subprocess.run(["export", "SLURM_MPI_TYPE=pmi2"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    #subprocess.run(["unset", "SLURM_EXPORT_ENV"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    return result


############
# - main - #
############


def main():
    # parse args
    parser = argparse.ArgumentParser(description='Generate Code from ExaSlang and run')
    parser.add_argument('exa_problem_path', type=str, help='Path to the ExaSlang problem specification')
    parser.add_argument('platform_path', type=str, help='Path to the platform description')
    args = parser.parse_args()

    # print arguments
    print(f"Executing: python3 slurm_alloc.py {' '.join(f'{k}={v}' for k, v in vars(args).items())}")

    # knowledge file assumed to be in the same source directory as the "*.exa?" files with same base name
    knowledge_path = remove_extension(get_exa_files(args.exa_problem_path)[0]) + '.knowledge'

    # compile target code
    exa_problem_name = get_problem_name_from_path(args.exa_problem_path)

    # parse knowledge and platform file
    config = ConfigFromKnowledge(exa_problem_name, knowledge_path, args.platform_path)
    print(config.__dict__)

    slurm_alloc(config)


if __name__ == "__main__":
    main()
