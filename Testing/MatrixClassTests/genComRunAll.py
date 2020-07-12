import timeit
import sys
import os
import subprocess
from subprocess import DEVNULL, STDOUT, check_call
from subprocess import Popen, PIPE
import ntpath

# test configs
tests = []

# application testcases
#tests.append("./NSGTestcase/2D_FD_Stokes_fromL4")
#tests.append("./NSGTestcase/3D_FD_Stokes_fromL4")
#tests.append("./NSGTestcase/3D_FV_NavierStokes_localNewton")
#tests.append("./NSGTestcase/3D_FV_NavierStokes_localPicard")
#tests.append("./NSGTestcase/3D_FV_Stokes_fromL4")
#tests.append("./NSGTestcase/2D_FD_OptFlow_fromL4")
#tests.append("./NSGTestcase/2D_FD_OptFlow_fromL4_Vec")

# compiletime inversion
tests.append("./invert/CompileTime/BlockDiagonal")
tests.append("./invert/CompileTime/Cofactors")
tests.append("./invert/CompileTime/Diagonal")
tests.append("./invert/CompileTime/GaussJordan")
tests.append("./invert/CompileTime/Schur")
tests.append("./invert/CompileTime/smallMatrices")

# runtime inversion
tests.append("./invert/Runtime/Schur")
tests.append("./invert/Runtime/LU")
tests.append("./invert/Runtime/Diagonal")
tests.append("./invert/Runtime/Blockdiagonal")
tests.append("./invert/Runtime/SchurLargeMatrix")
tests.append("./invert/Runtime/SmallMatrix")

# misc functions
tests.append("./resolvingMatrixFunctions/chaines")
tests.append("./resolvingMatrixFunctions/cross")
tests.append("./resolvingMatrixFunctions/determinant")
tests.append("./resolvingMatrixFunctions/dot")
tests.append("./resolvingMatrixFunctions/slicing")
tests.append("./resolvingMatrixFunctions/trace")
tests.append("./resolvingMatrixFunctions/transpose")

# arithmetic operators
tests.append("./resolvingMatrixOperators")


# solveLinearSystem
tests.append("./solveLinearSystem")

# determine structure
tests.append("./determineMatrixStructures")

# field declarations with matrix structure information
tests.append("./shapeFromField")





generator_path = "../../classes/artifacts/Compiler_jar/Compiler.jar"


def main():
    for test in tests:
        (dir_name,problem_name) = ntpath.split(test)
        #if not problem_name:
        #    problem_name = dir_name
        #print(dir_name)
        #print(problem_name)
        settings_path = f'{test}/{problem_name}.settings'
        knowledge_path = f'{test}/{problem_name}.knowledge'
        #print(settings_path)
        #print(knowledge_path)
        print("generating " + problem_name)
        genfile = open(f"{test}/Debug/gen_{problem_name}.txt",'w+') 
        out = subprocess.run(['java', '-cp', generator_path, 'Main',  settings_path, knowledge_path], stdout=genfile, stderr=genfile)
        if(out.returncode != 0): 
            print(problem_name + " failed while generating\n")
            continue
        
        # run makefile
        print("compiling " + problem_name)
        compilefile = open(f"{test}/Debug/compile_{problem_name}.txt",'w+') 
        out = subprocess.run(['make', '-B'],cwd=f"{test}/output/", stdout=compilefile, stderr=compilefile)
        if(out.returncode != 0): 
            print(problem_name + " failed while compiling\n")
            continue    

        # run application
        print("running " + problem_name + "\n")
        runfile = open(f"{test}/Debug/run_{problem_name}.txt",'w+') 
        out = subprocess.run(['./exastencils'], cwd=f"{test}/output/", stdout=runfile, stderr=runfile)
        if(out.returncode != 0): 
            print(problem_name + " failed while running\n")
            continue    
        # compare outputs with reference outputs

if __name__ == "__main__":
    main()