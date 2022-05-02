#!/usr/bin/env python3

import sys
import os
import subprocess
import functools
import time
import fnmatch
import argparse
import shutil
import errno


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


############################
# - generation functions - #
############################

# --- create name of problem from directory name --- #
def get_problem_name_from_path(exa_problem_path: str):
    base = get_exa_files(exa_problem_path)[0]
    return os.path.basename(remove_extension(base))


# --- create path for debug files --- #
def get_debug_base_path(exa_problem_path: str):
    return f"./Debug/{get_problem_name_from_path(exa_problem_path)}"


# --- remove file extension from path --- #
def remove_extension(path: str):
    return os.path.splitext(path)[0]


# --- get all "*.exa?" --- #
@functools.lru_cache(maxsize=None)
def get_exa_files(exa_problem_path: str):
    exa_files = []
    for file in fnmatch.filter(os.listdir(exa_problem_path), "*.exa?"):
        full_path = os.path.join(exa_problem_path, file)
        if os.path.isfile(full_path):
            exa_files = exa_files + [full_path]

    return exa_files


# --- generate ".settings" file for a directory with ExaSlang files --- #
def generate_settings_file(exa_problem_path: str, output_path: str):
    tmp = f'user\t= "Guest"\n\n'

    exa_files = get_exa_files(exa_problem_path)
    problem_name = get_problem_name_from_path(exa_problem_path)
    output_path_relative = os.path.relpath(output_path, exa_problem_path)

    if len(exa_files) < 1:
        print("No ExaSlang files found in directory: " + exa_problem_path)
        return -1

    tmp += f'basePathPrefix\t= "{exa_problem_path}"\n\n'
    for name in exa_files:
        for i in range(1, 5):
            if f'.exa{i}' in name:
                tmp += f'l{i}file\t = "{problem_name}.exa{i}"\n'

    tmp += f'debugL1File\t= "{get_debug_base_path(exa_problem_path)}_debug.exa1"\n'
    tmp += f'debugL2File\t= "{get_debug_base_path(exa_problem_path)}_debug.exa2"\n'
    tmp += f'debugL3File\t= "{get_debug_base_path(exa_problem_path)}_debug.exa3"\n'
    tmp += f'debugL4File\t= "{get_debug_base_path(exa_problem_path)}_debug.exa4"\n\n'
    tmp += f'htmlLogFile\t= "{get_debug_base_path(exa_problem_path)}_log.html"\n\n'
    tmp += f'outputPath\t= "{output_path_relative}"\n\n'
    tmp += f'produceHtmlLog\t= true\n'
    tmp += f'timeStrategies\t= true\n\n'
    # tmp += f'buildfileGenerators\t= {{"MakefileGenerator"}}\n'

    settings_path = f'{exa_problem_path}/{problem_name}.settings'
    with open(settings_path, "w") as file:
        print(tmp, file=file)

    return settings_path


# --- generate target code from ExaSlang specification with compiler --- #
@check_err
@timer
def generate_code(generator_path: str, generator_lib_path: str, settings_path: str, knowledge_path: str,
                  platform_path: str):
    return subprocess.run(
        ['java', '-cp', generator_path + ':' + generator_lib_path, 'Main', settings_path, knowledge_path,
         platform_path],
        stdout=subprocess.PIPE, stderr=subprocess.PIPE)


# --- conditionally generate ".settings" file --- #
def get_settings(exa_problem_path: str, output_path: str):
    settings = [n for n in fnmatch.filter(os.listdir(exa_problem_path), "*.settings") if
                os.path.isfile(os.path.join(exa_problem_path, n))]
    if len(settings) == 1:
        return os.path.join(exa_problem_path, settings[0])
    elif len(settings) > 1:
        print("Multiple setting files found: " + ", ".join(settings))
        return -1
    else:
        return generate_settings_file(exa_problem_path, output_path)


# --- copy files from src to dst --- #
def copy_files(src, dst):
    try:
        shutil.copytree(src, dst)
    except OSError as exc:
        if exc.errno == errno.ENOTDIR:
            shutil.copy(src, dst)
        else:
            raise

############
# - main - #
############


def main():
    # parse args
    parser = argparse.ArgumentParser(description='Generate Code from ExaSlang and run')
    parser.add_argument('generator_path', type=str, help='Path to the ExaStencils compiler')
    parser.add_argument('generator_lib_path', type=str, help='Path to the libraries required by the compiler')
    parser.add_argument('exa_source_path', type=str, help='Path to the ExaSlang problem specification')
    parser.add_argument('output_path', type=str, help='Path to ExaStencils output directory')
    parser.add_argument('platform_path', type=str, help='Path to the platform description')
    args = parser.parse_args()

    # print arguments
    print(f"Executing: python3 runBenchmark.py {' '.join(f'{k}={v}' for k, v in vars(args).items())}")

    # get settings
    settings_path = get_settings(args.exa_source_path, args.output_path)
    if settings_path == -1:
        sys.exit(settings_path)

    # knowledge file assumed to be in the same source directory as the "*.exa?" files with same base name
    knowledge_path = remove_extension(get_exa_files(args.exa_source_path)[0]) + '.knowledge'

    # debug path
    debug_path = os.path.join(args.exa_source_path, "Debug")
    if not os.path.exists(debug_path):
        os.mkdir(debug_path)

    # call compiler
    generate_code(args.generator_path, args.generator_lib_path, settings_path, knowledge_path, args.platform_path)


if __name__ == "__main__":
    main()
