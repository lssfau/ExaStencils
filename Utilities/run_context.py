#!/usr/bin/env python3

import os
from generation_helpers import *
from config_from_knowledge import *

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

        # libs
        self.use_likwid = getattr(args, "use_likwid", False)
        self.use_likwid_perfctr = getattr(args, "use_likwid_perfctr", False)
        self.use_likwid_pin = getattr(args, "use_likwid_pin", False)
        if not self.use_likwid and self.use_likwid_perfctr:
            raise ValueError('use_likwid_perfctr is only available in conjunction with use_likwid.')

        # set run options
        all = getattr(args, "all", False)
        self.generate = getattr(args, "generate", False) or all
        self.compile = getattr(args, "compile", False) or all
        self.run = getattr(args, "run", False) or all

        # get list of exaslang source paths
        exa_files = tuple(args.exaslang_files.split(';'))
        self.exa_files = get_exa_file_paths(self.exa_problem_path, exa_files)

        # get path to knowledge file
        self.knowledge_path = get_file_in_problem_path(self.exa_problem_path, self.knowledge_filename)

        # use platform name as suffix
        self.platform_suffix = os.path.basename(remove_extension(self.platform_path))
        self.target_code_path = get_target_code_path(self.output_path, self.problem_name, self.platform_suffix)

        # parse knowledge and platform file
        self.config = ConfigFromKnowledge(self.problem_name, self.knowledge_path, self.platform_path)

        # get/generate settings file
        if not self.settings_file:
            # settings file automatically generated
            self.settings_path = generate_settings_file(self.exa_files, self.exa_problem_path, self.problem_name,
                                                        self.output_path, self.target_code_path, self.overwrite_settings,
                                                        self.config.use_cuda, self.use_likwid, self.use_likwid_perfctr)
        else:
            # settings file specified by user
            self.settings_path = get_file_in_problem_path(self.exa_problem_path, self.settings_file)
