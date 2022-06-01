#!/usr/bin/env python3

import os
import subprocess
import argparse
from run_context import *
from decorators import *
from likwid_pinning import *


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
def run_code(ctx: RunContext, config: ConfigFromKnowledge, use_likwid_pin: bool = False):
    cwd = os.getcwd()
    os.chdir(ctx.target_code_path)

    if use_likwid_pin:
        # run code with likwid pinning
        pinned_exec = likwid_pin(config)
        print(pinned_exec)
        result = subprocess.run(pinned_exec, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    else:
        # run with mpirun
        result = subprocess.run([f'mpirun', '--allow-run-as-root', '--oversubscribe', '--mca', 'btl_base_warn_component_unused', '0',
                                 f'-np', f'{config.mpi_num_processes}', 'exastencils'], stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    # print stdout
    print(result.stderr.decode('utf-8'))
    os.chdir(cwd)

    return result


############
# - main - #
############


def main():
    # parse args
    parser = argparse.ArgumentParser(description='Generate Code from ExaSlang and run')
    parser.add_argument('problem_name', type=str, help='Name of the problem. Used as directory name for the generated code')
    parser.add_argument('exa_problem_path', type=str, help='Path to the ExaSlang problem specification')
    parser.add_argument('exaslang_files', type=str, help='Comma-separated ExaSlang path assumed to be in \"exa_problem_path\"')
    parser.add_argument('platform_path', type=str, help='Path to the platform description')
    parser.add_argument('knowledge_file', type=str, help='Knowledge path assumed to be in \"exa_problem_path\"')
    parser.add_argument('output_path', type=str, help='Path to output directory')
    parser.add_argument('--settings_file', type=str, default=default_args['settings_file'],
                        help='Settings path assumed to be in \"exa_problem_path\". Generated otherwise')
    parser.add_argument('--generator_path', type=str, default=default_args['generator_path'],
                        help='Path to the ExaStencils compiler')
    parser.add_argument('--generator_lib_path', type=str, default=default_args['generator_lib_path'],
                        help='Path to the libraries required by the compiler')
    parser.add_argument('--overwrite_settings', action='store_true', default=default_args['overwrite_settings'],
                        help='Generate target code from ExaSlang')
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
        # parse knowledge and platform file
        config = ConfigFromKnowledge(ctx.problem_name, ctx.knowledge_path, ctx.platform_path)

        # run target code
        run_code(ctx, config)


if __name__ == "__main__":
    main()
