#!/usr/bin/env python3

import os
import subprocess
import argparse
from run_context import *
from decorators import *
from likwid_pinning import *
from argparse_helpers import *


#################################
# - gen/compile/run functions - #
#################################


# --- generate target code from ExaSlang specification --- #
@check_err
@timer
def generate_code(ctx: RunContext):
    return subprocess.run(
        ['java', '-cp', ctx.generator_path + ':' + ctx.generator_lib_path, 'Main', ctx.settings_path, ctx.knowledge_path,
         ctx.platform_path],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE)


# --- compile target code --- #
@check_err
@timer
def compile_code(ctx: RunContext):
    return subprocess.run(['make', '-j', '-s', '-C', ctx.target_code_path],
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE)


# --- run target code --- #
@check_err
@timer
def run_code(ctx: RunContext):
    cwd = os.getcwd()
    os.chdir(ctx.target_code_path)

    if ctx.use_likwid_pin:
        # run code with likwid pinning
        pinned_exec = likwid_pin(ctx.config, ctx.use_likwid_perfctr)
        print(pinned_exec)
        result = subprocess.run(pinned_exec, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    else:
        # run with mpirun
        bin = ['./exastencils']
        if ctx.use_likwid_perfctr:
            bin = ['likwid-perfctr', '-m', '-g', 'FLOPS_DP'] + bin
        exec_as_root = ['--allow-run-as-root'] if ctx.mpi_run_as_root else []
        mpi_run = [f'mpirun'] + exec_as_root + ['--oversubscribe', '--mca', 'btl_base_warn_component_unused', '0',
                    f'-np', f'{ctx.config.mpi_num_processes}'] + bin
        if ctx.config.mpi_enabled:
            bin = mpi_run
        print(bin)
        result = subprocess.run(bin, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    # print stderr
    print(result.stderr.decode('utf-8'))
    os.chdir(cwd)

    return result


############
# - main - #
############


def main():
    # parse args
    parser = argparse.ArgumentParser(description='Generate Code from ExaSlang and run')
    parser.add_argument('problem_name', type=str,
                        help='Name of the problem. Used as directory name for the generated code')
    parser.add_argument('exa_problem_path', type=str, help='Path to the ExaSlang problem specification')
    parser.add_argument('exaslang_files', type=str,
                        help='Comma-separated ExaSlang path assumed to be in \"exa_problem_path\"')
    parser.add_argument('knowledge_file', type=str, help='Knowledge path assumed to be in \"exa_problem_path\"')
    parser.add_argument('platform_path', type=str, help='Path to the platform description')
    parser.add_argument('output_path', type=str, help='Path to output directory')
    parser.add_argument('--settings_file', type=str, default=default_args['settings_file'],
                        help='Settings path assumed to be in \"exa_problem_path\". Generated otherwise')
    parser.add_argument('--generator_path', type=str, default=default_args['generator_path'],
                        help='Path to the ExaStencils compiler')
    parser.add_argument('--generator_lib_path', type=str, default=default_args['generator_lib_path'],
                        help='Path to the libraries required by the compiler')
    parser.add_argument('--overwrite_settings', type=str_to_bool, nargs='?', const=True, default=False,
                        help='Generate target code from ExaSlang')
    parser.add_argument('--use_likwid', default=False, action='store_true',
                        help='Use likwid for benchmarks')
    parser.add_argument('--use_likwid_perfctr', default=False, action='store_true',
                        help='Activate performance counters of likwid')
    parser.add_argument('--use_likwid_pin', default=False, action='store_true',
                        help='Use "likwid-pin" for code execution')
    parser.add_argument('--mpi_run_as_root', default=False, action='store_true',
                        help='Use "--mpi_run_as_root" option for mpirun')
    parser.add_argument('--generate', action='store_true', help='Generate target code from ExaSlang')
    parser.add_argument('--compile', action='store_true', help='Compile generated target code')
    parser.add_argument('--run', action='store_true', help='Run generated target code')
    parser.add_argument('--all', action='store_true', help='Generate, compile and run target code')
    args = parser.parse_args()

    # print arguments
    print(f"Executing: python3 run_exastencils.py {' '.join(f'{k}={v}' for k, v in vars(args).items())}")

    # set run options
    ctx = RunContext(args)

    if ctx.generate:
        # generate target code
        generate_code(ctx)

    if ctx.compile:
        # compile target code
        compile_code(ctx)

    if ctx.run:
        # run target code
        run_code(ctx)


if __name__ == "__main__":
    main()
