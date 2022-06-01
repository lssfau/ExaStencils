#!/usr/bin/env python3

import os
from generation_helpers import *

default_args = {
    'settings_file': '',
    'generator_path': '../Compiler/Compiler.jar',
    'generator_lib_path': '../Compiler/lib',
    'overwrite_settings': False,
    'json_influx_file': 'results.json'
}


class RunContext:
    def __init__(self, args):
        # copy over
        self.exa_problem_path = args.exa_problem_path
        self.platform_path = args.platform_path
        self.knowledge_filename = args.knowledge_file
        self.problem_name = args.problem_name
        self.output_path = args.output_path

        self.settings_file = getattr(args, "settings_file", default_args['settings_file'])
        self.generator_path = getattr(args, "generator_path", default_args['generator_path'])
        self.generator_lib_path = getattr(args, "generator_lib_path", default_args['generator_lib_path'])
        self.overwrite_settings = getattr(args, "overwrite_settings", default_args['overwrite_settings'])

        # set run options
        self.generate = args.generate or args.all
        self.compile = args.compile or args.all
        self.run = args.generate or args.all

        # get list of exaslang source paths
        self.exa_files = args.exaslang_files.split(',')

        # get path to knowledge file
        self.knowledge_path = get_file_in_problem_path(self.exa_problem_path, self.knowledge_filename)

        # use platform name as suffix
        self.platform_suffix = os.path.basename(remove_extension(self.platform_path))
        self.target_code_path = get_target_code_path(self.output_path, self.problem_name, self.platform_suffix)

        if not self.settings_file:
            # settings file automatically generated
            self.settings_path = generate_settings_file(self.exa_files, self.exa_problem_path, self.problem_name,
                                                        self.output_path, self.target_code_path, self.overwrite_settings)
        else:
            # settings file specified by user
            self.settings_path = get_file_in_problem_path(self.exa_problem_path, self.settings_file)
