#!/usr/bin/env python3

default_args = {
    'settings_file': '',
    'generator_path': '../Compiler/Compiler.jar',
    'generator_lib_path': '../Compiler/lib',
    'overwrite_settings': False,
    'json_influx_file': 'results.json'
}


def str_to_bool(value):
    if value.lower() in {'false', 'f', '0', 'no', 'n'}:
        return False
    elif value.lower() in {'true', 't', '1', 'yes', 'y'}:
        return True
    raise ValueError(f'{value} is not a valid boolean value')
