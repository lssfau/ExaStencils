#!/usr/bin/env python3

import sys
import functools
import time


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

def check_err(suppress_stdout):
    def check_err_decorator(func):
        @functools.wraps(func)
        def wrapper_check_err(*args, **kwargs):
            result = func(*args, **kwargs)
            if not result.returncode == 0:
                print(f"ERROR: Function '{func.__name__}' returned code {result.returncode}")
                print("---- STDERR: ----")
                print(result.stderr.decode('utf-8'))
                print("---- STDOUT: ----")
                print(result.stdout.decode('utf-8'))
                sys.exit(result)
            else:
                print(f"Function '{func.__name__}' has exited successfully")
                if not suppress_stdout:
                    print("---- STDOUT: ----")
                    print(result.stdout.decode('utf-8'))
            return result
        return wrapper_check_err
    return check_err_decorator
