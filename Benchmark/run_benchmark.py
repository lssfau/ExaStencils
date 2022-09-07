#!/usr/bin/env python3

import os
import argparse
import json
import copy
import shutil
from generation_helpers import *
from run_context import *
from upload_grafana import *
from extend_json_body import *

import sys

sys.path.append("../Utilities")
from run_exastencils import *


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
        run_code(ctx, use_likwid_pin=True)

        # upload to grafana
        if os.path.exists(json_file):
            f = open(json_file)
            json_body = json.load(f)
            up = UploadGrafana(decorate_json(json_body, ctx.config))
        else:
            print('Grafana upload failed. No JSON file found: ' + json_file)


############
# - main - #
############


def main():
    # parse args
    parser = argparse.ArgumentParser(description='Generate Benchmark from ExaSlang and run')
    parser.add_argument('problem_name', type=str, help='Name of the problem. Used as directory name for the generated code')
    parser.add_argument('exa_problem_path', type=str, help='Path to the ExaSlang problem specification')
    parser.add_argument('exaslang_files', type=str, help='Comma-separated ExaSlang path assumed to be in \"exa_problem_path\"')
    parser.add_argument('knowledge_file', type=str, help='Knowledge path assumed to be in \"exa_problem_path\"')
    parser.add_argument('platform_path', type=str, help='Path to the platform description')
    parser.add_argument('output_path', type=str, help='Path to output directory')
    parser.add_argument('--json_influx_file', type=str, default=default_args['json_influx_file'],
                        help='JSON filename output by target code and used as input for the InfluxDB upload')
    parser.add_argument('--overwrite_settings', action='store_true', default=True, help='Generate target code from ExaSlang')
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

    if not ctx_base.config.use_cuda:
        # set run options: enable OpenMP with "numCoresPerCPU" threads
        new_args = copy.deepcopy(args)
        par_knowledge_path = f"{remove_extension(args.knowledge_file)}_OMP.knowledge"
        shutil.copyfile(args.knowledge_file, par_knowledge_path)
        with open(par_knowledge_path, 'a+') as new_knowledge:
            new_knowledge.append(f"omp_enabled = true")
            new_knowledge.append(f"omp_numThreads = {ctx_base.config.cores_per_cpu}")
        new_args.knowledge_file = par_knowledge_path

        # generate code to new location
        new_args.problem_name += "_OMP"
        new_args.output_path += "_OMP"

        # create adapted run context
        ctx_parallel = RunContext(args)
        json_file = f'{ctx_parallel.target_code_path}/{args.json_influx_file}'

        # run pipeline
        run_pipeline(ctx_parallel, json_file)


if __name__ == "__main__":
    main()
