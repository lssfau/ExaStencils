import timeit
import sys
import os
import subprocess
from subprocess import DEVNULL, STDOUT, check_call
from subprocess import Popen, PIPE
import ntpath

# test configs
tests = ["./NSGTestcase/2D_FD_OptFlow_fromL4"]
tests.append("./NSGTestcase/2D_FD_Stokes_fromL4")
tests.append("./NSGTestcase/3D_FV_NavierStokes_localNewton")
tests.append("./NSGTestcase/3D_FV_NavierStokes_localPicard")

generator_path_new = "../../out/artifacts/Compiler_jar/Compiler.jar"
generator_path_old = "../../../refc/exastencils/classes/artifacts/Compiler_jar/Compiler.jar"


def main():
    for test in tests:
        problem_name = ntpath.basename(test)
        settings_path = f'{test}/{problem_name}.settings'
        knowledge_path = f'{test}/{problem_name}.knowledge'
        print("generating " + problem_name + " with new configuartion ...")
        dbgfile_new = open(f"{test}/Debug/{problem_name}_debug_new.txt",'w+') #same with "w" or "a" as opening mode
        subprocess.run(['java', '-cp', generator_path_new, 'Main',  settings_path, knowledge_path], stdout=dbgfile_new, stderr=dbgfile_new, stdin=PIPE)

        print("\n\n\ngenerating " + problem_name + " with old configuartion ...")
        dbgfile_old = open(f"{test}/Debug/{problem_name}_debug_old.txt",'w+') #same with "w" or "a" as opening mode
        subprocess.run(['java', '-cp', generator_path_new, 'Main',  settings_path, knowledge_path], stdout=dbgfile_old, stderr=dbgfile_old, stdin=PIPE)


if __name__ == "__main__":
    main()
