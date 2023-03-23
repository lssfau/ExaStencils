#!/usr/bin/env python3

import argparse
import sys

sys.path.append("../Utilities")
from run_exastencils import *
from generation_helpers import *
from argparse_helpers import *


def check_results(result_str: str, expected_results_path: str):
    EPS = 1e-6

    def print_results(generated, expected):
        
        print('Expected:\t\tGenerated:')
        for s1, s2 in zip(expected, generated):
            print(f'{s1.strip()}\t{s2.strip()}')
    with open(expected_results_path, 'r') as file:
        expected_results = file.readlines()
        expected_results = [x.strip() for x in expected_results]
        results = result_str.splitlines()
        for s1, s2 in zip(results, expected_results):
            s1 = s1.strip()
            s2 = s2.strip()
            if s1 != s2:
                try:
                    if abs(float(s1) - float(s2)) > EPS:
                        print(f'Results do not match.')
                        print(f'Expected "{s2.strip()}" but got "{s1.strip()}"')
                        print('\nFull comparison:')
                        print_results(results, expected_results)
                        return False
                except ValueError as ve:
                    print(f'Results do not match.')
                    print(f'Expected "{s2.strip()}" but got "{s1.strip()}"')
                    print('\nFull comparison:')
                    print_results(results, expected_results)
                    return False

    return True


def main():
    # parse args
    parser = argparse.ArgumentParser(description='Generate Benchmark from ExaSlang and run')
    parser.add_argument('problem_name', type=str, help='Name of the problem. Used as directory name for the generated code')
    parser.add_argument('exa_problem_path', type=str, help='Path to the ExaSlang problem specification')
    parser.add_argument('knowledge_file', type=str, help='Knowledge path assumed to be in \"exa_problem_path\"')
    parser.add_argument('exaslang_files', type=str, help='Comma-separated ExaSlang paths assumed to be in \"exa_problem_path\"')
    parser.add_argument('expected_results', type=str, help='Path to file with expected results. Assumed to be in \"exa_problem_path\"')
    parser.add_argument('platform_path', type=str, help='Path to the platform description')
    parser.add_argument('output_path', type=str, help='Path to output directory')
    parser.add_argument('--overwrite_settings', type=str_to_bool, nargs='?', const=True, default=True,
                        help='Generate target code from ExaSlang')
    parser.add_argument('--mpi_run_as_root', default=True,
                        help='Use "--mpi_run_as_root" option for mpirun')
    parser.add_argument('--no_mpi_run_as_root', dest='mpi_run_as_root', action='store_false',
                        help='Omit "--mpi_run_as_root" option for mpirun')
    args = parser.parse_args()

    # print arguments
    print(f"Executing: python3 run_test.py {' '.join(f'{k}={v}' for k, v in vars(args).items())}")

    # set run options
    ctx = RunContext(args)

    # Fix for strange behavior of PolyExpl tests
    if "PolyExpl" in ctx.problem_name:
        ctx.target_code_path += "00000"

    # generate target code
    generate_code(ctx)

    # compile target code
    compile_code(ctx)

    # mkdir "data" dir for IOTests
    os.makedirs(os.path.join(ctx.target_code_path, "data"), exist_ok=True)

    # run target code
    result = run_code(ctx)

    # compare with expecred results
    if args.expected_results:
        result_str = result.stdout.decode('utf-8')
        if check_results(result_str, get_file_in_problem_path(ctx.exa_problem_path, args.expected_results)) is True:
            print(f"Test for problem \"{ctx.problem_name}\" finished successfully.")
            return result.returncode
        else:
            return -1


if __name__ == "__main__":
    main()
