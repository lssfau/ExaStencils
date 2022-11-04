#!/usr/bin/env python3

import os
import argparse
import json
import copy
import shutil
from upload_grafana import *
from extend_json_body import *

import sys

sys.path.append("../Utilities")
from run_exastencils import *
from generation_helpers import *
from argparse_helpers import *
from run_context import *


# generate, compile, run and upload
def run_pipeline(ctx: RunContext, json_file: str):
    if ctx.generate:
        # generate target code
        generate_code(ctx)

    if ctx.compile:
        # compile target code
        compile_code(ctx)

    if ctx.run:
        # run target code
        result = run_code(ctx)

        # extend json body and upload to grafana
        if os.path.exists(json_file):
            f = open(json_file)
            json_body = json.load(f)

            if ctx.use_likwid and ctx.use_likwid_perfctr:
                json_body = parse_likwid_perfctr_output(result.stdout, json_body)

            up = UploadGrafana(decorate_json(json_body, ctx.config))
        else:
            print('Grafana upload failed. No JSON file found: ' + json_file)


############
# - main - #
############


def main():
    # parse args
    parser = argparse.ArgumentParser(description='Generate Benchmark from ExaSlang and run')
    parser.add_argument('problem_name', type=str,
                        help='Name of the problem. Used as directory name for the generated code')
    parser.add_argument('exa_problem_path', type=str, help='Path to the ExaSlang problem specification')
    parser.add_argument('exaslang_files', type=str,
                        help='Comma-separated ExaSlang path assumed to be in \"exa_problem_path\"')
    parser.add_argument('knowledge_file', type=str, help='Knowledge path assumed to be in \"exa_problem_path\"')
    parser.add_argument('platform_path', type=str, help='Path to the platform description')
    parser.add_argument('output_path', type=str, help='Path to output directory')
    parser.add_argument('--overwrite_settings', type=str_to_bool, nargs='?', const=True, default=True,
                        help='Generate target code from ExaSlang')
    parser.add_argument('--use_likwid', default=False, action='store_true',
                        help='Use likwid for benchmarks')
    parser.add_argument('--use_likwid_perfctr', default=False, action='store_true',
                        help='Activate performance counters of likwid')
    parser.add_argument('--use_likwid_pin', default=False, action='store_true',
                        help='Use "likwid-pin" for code execution')
    parser.add_argument('--json_influx_file', type=str, default=default_args['json_influx_file'],
                        help='JSON filename output by target code and used as input for the InfluxDB upload')
    parser.add_argument('--generate', action='store_true', help='Generate target code from ExaSlang')
    parser.add_argument('--compile', action='store_true', help='Compile generated target code')
    parser.add_argument('--run', action='store_true', help='Run generated target code')
    parser.add_argument('--all', action='store_true', help='Generate, compile and run target code')
    args = parser.parse_args()

    # print arguments
    print(f"Executing: python3 run_benchmark.py {' '.join(f'{k}={v}' for k, v in vars(args).items())}")

    # skip if platform file does not exist
    if not os.path.isfile(args.platform_path):
        print("Skipping host without corresponding platform file: " + args.platform_path)
        return

    ###############
    # base config #
    ###############

    # set run options
    ctx_base = RunContext(args)
    json_file = f'{ctx_base.target_code_path}/{args.json_influx_file}'

    # run pipeline
    run_pipeline(ctx_base, json_file)

    ##########################
    # OpenMP parallel config #
    ##########################

    # for CPU applications: rerun pipeline with adapted input args
    if not ctx_base.config.use_cuda:
        # copy and adapt input args
        suffix = "OMP"
        platform_suffix = ctx_base.platform_suffix
        knowledge_base = os.path.basename(remove_extension(args.knowledge_file))
        new_args = copy.deepcopy(args)
        new_args.problem_name += f"_{suffix}"
        new_args.knowledge_file = f"{knowledge_base}_{suffix}_{platform_suffix}.knowledge"

        # copy exaslang sources to new directory and adapt knowledge file
        if new_args.generate:
            with open(f"{new_args.exa_problem_path}/{new_args.knowledge_file}", 'a') as new_knowledge:
                # enable OpenMP with "numCoresPerCPU" threads
                new_knowledge.write(f"\nimport '{args.knowledge_file}'")
                new_knowledge.write(f"\nomp_enabled = true")
                new_knowledge.write(f"\nomp_numThreads = {ctx_base.config.cores_per_cpu}")

        # create adapted run context
        ctx_omp = RunContext(new_args)
        json_file = f'{ctx_omp.target_code_path}/{new_args.json_influx_file}'

        # run pipeline
        run_pipeline(ctx_omp, json_file)


if __name__ == "__main__":
    main()
