#!/usr/bin/env python3

import os
import functools
import fnmatch


##########################
# - generation helpers - #
##########################

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
    exa_problem_name = get_problem_name_from_path(exa_problem_path)
    output_path_relative = os.path.relpath(output_path, exa_problem_path)

    if len(exa_files) < 1:
        print("No ExaSlang files found in directory: " + exa_problem_path)
        return -1

    tmp += f'basePathPrefix\t= "{exa_problem_path}"\n\n'
    for name in exa_files:
        for i in range(1, 5):
            if f'.exa{i}' in name:
                tmp += f'l{i}file\t = "{exa_problem_name}.exa{i}"\n'

    tmp += f'debugL1File\t= "{output_path_relative}/{get_debug_base_path(exa_problem_path)}_debug.exa1"\n'
    tmp += f'debugL2File\t= "{output_path_relative}/{get_debug_base_path(exa_problem_path)}_debug.exa2"\n'
    tmp += f'debugL3File\t= "{output_path_relative}/{get_debug_base_path(exa_problem_path)}_debug.exa3"\n'
    tmp += f'debugL4File\t= "{output_path_relative}/{get_debug_base_path(exa_problem_path)}_debug.exa4"\n\n'
    tmp += f'htmlLogFile\t= "{output_path_relative}/{get_debug_base_path(exa_problem_path)}_log.html"\n\n'
    tmp += f'outputPath\t= "{output_path_relative}/generated/{exa_problem_name}"\n\n'
    tmp += f'produceHtmlLog\t= true\n'
    tmp += f'timeStrategies\t= true\n\n'
    tmp += f'buildfileGenerators\t= {{"MakefileGenerator"}}\n'

    settings_path = f'{exa_problem_path}/{exa_problem_name}.settings'
    with open(settings_path, "w") as file:
        print(tmp, file=file)

    return settings_path
