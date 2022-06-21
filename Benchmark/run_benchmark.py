#!/usr/bin/env python3

import os
import argparse
import json
from upload_grafana import *
from extend_json_body import *

import sys

sys.path.append("../Utilities")
from run_exastencils import *


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
        run_code(ctx, config, use_likwid_pin=True)

        # upload to grafana
        json_file = f'{ctx.target_code_path}/{args.json_influx_file}'
        if os.path.exists(json_file):
            f = open(json_file)
            json_body = json.load(f)
            up = UploadGrafana(decorate_json(json_body, config))
        else:
            print('Grafana upload failed. No JSON file found: ' + json_file)


if __name__ == "__main__":
    main()
