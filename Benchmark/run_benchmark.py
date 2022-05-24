#!/usr/bin/env python3

import sys
import re
import os
import subprocess
import functools
import time
import fnmatch
import argparse
import shutil
import errno
import json
from upload_grafana import *
from generation_helpers import *
from decorate_json import *
from likwid_pinning import *


##################
# - decorators - #
##################

# --- time function --- #
def timer(func):
    @functools.wraps(func)
    def wrapper_timer(*args, **kwargs):
        start_time = time.perf_counter()
        value = func(*args, **kwargs)
        end_time = time.perf_counter()
        run_time = end_time - start_time
        print(f"Finished {func.__name__!r} in {run_time:.4f} seconds.")
        return value

    return wrapper_timer


# --- error code check --- #

def check_err(func):
    @functools.wraps(func)
    def wrapper_check_err(*args, **kwargs):
        result = func(*args, **kwargs)
        if not result.returncode == 0:
            print(result.stderr.decode('utf-8'))
            sys.exit(result)
        return result.returncode

    return wrapper_check_err


# --- generate target code from ExaSlang specification with compiler --- #
@check_err
@timer
def generate_code(generator_path: str, generator_lib_path: str, settings_path: str, knowledge_path: str,
                  platform_path: str):
    return subprocess.run(
        ['java', '-cp', generator_path + ':' + generator_lib_path, 'Main', settings_path, knowledge_path,
         platform_path],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE)


# --- copy files from src to dst --- #
def copy_files(src, dst):
    try:
        shutil.copytree(src, dst)
    except OSError as exc:
        if exc.errno == errno.ENOTDIR:
            shutil.copy(src, dst)
        else:
            raise


#####################
# - run functions - #
#####################

@check_err
@timer
def compile_benchmark(target_code_path: str):
    return subprocess.run(['make', '-j', '-s', '-C', target_code_path],
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE)


@check_err
@timer
def run_benchmark(target_code_path: str, config: ConfigFromKnowledge):
    cwd = os.getcwd()
    os.chdir(target_code_path)

    # run code with likwid pinning
    pinned_exec = likwid_pin(config)
    print(pinned_exec)
    result = subprocess.run(pinned_exec, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

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
    parser.add_argument('generator_path', type=str, help='Path to the ExaStencils compiler')
    parser.add_argument('generator_lib_path', type=str, help='Path to the libraries required by the compiler')
    parser.add_argument('exa_problem_path', type=str, help='Path to the ExaSlang problem specification')
    parser.add_argument('output_path', type=str, help='Path to ExaStencils output directory')
    parser.add_argument('platform_path', type=str, help='Path to the platform description')
    parser.add_argument('--generate', action='store_true', help='Generate target code from ExaSlang')
    parser.add_argument('--compile', action='store_true', help='Compile generated target code')
    parser.add_argument('--run', action='store_true', help='Run generated target code')
    parser.add_argument('--all', action='store_true', help='Generate, compile and run target code')
    args = parser.parse_args()

    # print arguments
    print(f"Executing: python3 run_benchmark.py {' '.join(f'{k}={v}' for k, v in vars(args).items())}")
    generate = args.generate or args.all
    compile = args.compile or args.all
    run = args.run or args.all

    exa_problem_name = get_problem_name_from_path(args.exa_problem_path)
    platform_suffix = os.path.basename(remove_extension(args.platform_path))
    target_code_path = f"{args.output_path}/generated/{exa_problem_name}_{platform_suffix}"

    # knowledge file assumed to be in the same source directory as the "*.exa?" files with same base name
    knowledge_path = remove_extension(get_exa_files(args.exa_problem_path)[0]) + '.knowledge'

    if generate:
        # get settings
        settings_path = generate_settings_file(args.exa_problem_path, args.output_path, target_code_path)

        # generate target code
        generate_code(args.generator_path, args.generator_lib_path, settings_path, knowledge_path, args.platform_path)

    if compile:
        # compile target code
        compile_benchmark(target_code_path)

    if run:
        # parse knowledge and platform file
        config = ConfigFromKnowledge(exa_problem_name, knowledge_path, args.platform_path)

        # run target code
        run_benchmark(target_code_path, config)

        # upload to grafana
        json_file = f'{target_code_path}/results.json'
        if os.path.exists(json_file):
            f = open(json_file)
            json_body = json.load(f)
            up = UploadGrafana(decorate_json(json_body, config))
        else:
            print('Grafana upload failed. No JSON file found: ' + json_file)


if __name__ == "__main__":
    main()
