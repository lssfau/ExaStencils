#!/usr/bin/env python3

import os
import re
import functools
import fnmatch
import shutil
import errno
from typing import List


##########################
# - generation helpers - #
##########################

# --- remove file extension from path --- #
def remove_extension(path: str):
    return os.path.splitext(path)[0]


# --- get all "*.exa?" files --- #
@functools.lru_cache(maxsize=None)
def get_exa_file_paths(exa_problem_path: str, exa_files: List[str]):
    collected_files = []

    # iterate over specified exaslang paths (potentially with regex)
    for exa_file in exa_files:
        # go to exaslang path (relative from exa_problem_path)
        exa_file_path = os.path.join(exa_problem_path, os.path.dirname(exa_file))

        # match for all "*.exa?" occurrences in exaslang path
        for file in fnmatch.filter(os.listdir(exa_file_path), "*.exa?"):
            basename_exa_file = os.path.basename(exa_file)
            # check if matched file corresponds to specified exaslang file (potentially with regex)
            if re.match(basename_exa_file, file) or basename_exa_file == file:
                path_found_file = os.path.join(os.path.dirname(exa_file), file)
                collected_files = collected_files + [path_found_file]

    return collected_files


# --- copy files from src to dst --- #
def copy_files(src, dst):
    try:
        shutil.copytree(src, dst)
    except OSError as exc:
        if exc.errno == errno.ENOTDIR:
            shutil.copy(src, dst)
        else:
            raise


# --- strip C/C++ like comments from string --- #
def strip_comments(file_contents: str):
    def replacer(match):
        s = match.group(0)
        if s.startswith('/'):
            return " "  # note: a space and not an empty string
        else:
            return s

    pattern = re.compile(
        r'//.*?$|/\*.*?\*/|\'(?:\\.|[^\\\'])*\'|"(?:\\.|[^\\"])*"',
        re.DOTALL | re.MULTILINE
    )
    return re.sub(pattern, replacer, file_contents)


# --- get path of generated target code --- #
def get_target_code_path(output_path: str, problem_name: str, platform_suffix: str):
    return f"{output_path}/generated/{problem_name}_{platform_suffix}"


# --- get path of file in exa_problem_path and check if exists --- "
def get_file_in_problem_path(exa_problem_path: str, filename: str, suppress: bool = False):
    # file assumed to be in exa_problem_path (= same directory as the "*.exa?" files)
    file = f"{exa_problem_path}/{filename}"
    if not os.path.isfile(file) and not suppress:
        raise ValueError(f"Specified file {filename} does not exist in {exa_problem_path}")
    return file


# --- generate ".settings" file for a directory with ExaSlang files --- #
def generate_settings_file(exa_files: List[str], exa_problem_path: str, problem_name: str, output_path: str,
                           target_code_path: str, overwrite: bool, use_cuda: bool, use_likwid: bool, use_likwid_perfctr: bool):
    # ignore warning that file does not exist, because it is yet to be created
    settings_path = get_file_in_problem_path(exa_problem_path, f"{problem_name}.settings", suppress=True)

    if not os.path.isfile(settings_path) or overwrite:
        print_settings_file(exa_files, exa_problem_path, problem_name,
                            settings_path, output_path, target_code_path, use_cuda, use_likwid, use_likwid_perfctr)
    else:
        raise ValueError(
            f"File {settings_path} already exists. Run again with the --overwrite_settings flag to overwrite.")

    return settings_path


def print_settings_file(exa_files: List[str], exa_problem_path: str, exa_problem_name: str, settings_path: str,
                        output_path: str, target_code_path: str, use_cuda: bool, use_likwid: bool, use_likwid_perfctr: bool):
    tmp = f'user\t= "Guest"\n\n'

    output_path_relative = os.path.relpath(output_path, exa_problem_path)
    target_code_path_relative = os.path.relpath(target_code_path, exa_problem_path)

    if len(exa_files) < 1:
        print("No ExaSlang files found in directory: " + exa_problem_path)
        return -1

    tmp += f'basePathPrefix\t= "{exa_problem_path}"\n\n'
    for name in exa_files:
        for i in range(1, 5):
            if f'.exa{i}' in name:
                tmp += f'l{i}file\t = "{name}"\n'

    debug_base = f"./Debug/{exa_problem_name}"

    tmp += f'debugL1File\t= "{output_path_relative}/{debug_base}_debug.exa1"\n'
    tmp += f'debugL2File\t= "{output_path_relative}/{debug_base}_debug.exa2"\n'
    tmp += f'debugL3File\t= "{output_path_relative}/{debug_base}_debug.exa3"\n'
    tmp += f'debugL4File\t= "{output_path_relative}/{debug_base}_debug.exa4"\n\n'
    tmp += f'htmlLogFile\t= "{output_path_relative}/{debug_base}_log.html"\n\n'
    tmp += f'outputPath\t= "{target_code_path_relative}"\n\n'
    tmp += f'produceHtmlLog\t= true\n'
    tmp += f'timeStrategies\t= true\n\n'
    tmp += f'buildfileGenerators\t= {{"MakefileGenerator"}}\n\n'

    if use_cuda:
        tmp += f'pathsInc += "/usr/local/cuda/include"\n'
        tmp += f'pathsLib += "/usr/local/cuda/lib64"\n'

    if use_likwid:
        tmp += f'pathsLib += "$(shell dirname $(shell which likwid-perfctr))/../lib/"\n'
        tmp += f'pathsInc += "$(shell dirname $(shell which likwid-perfctr))/../include/"\n'
        tmp += f'additionalLibs      += "likwid"\n'

        if use_likwid_perfctr:
            tmp += f'additionalIncludes  += "likwid-marker.h"\n'
            tmp += f'additionalDefines   += "LIKWID_PERFMON"\n'

    with open(settings_path, "w") as file:
        print(tmp, file=file)
