import timeit
import sys
import os
import subprocess
from subprocess import DEVNULL, STDOUT, check_call
from subprocess import Popen, PIPE
import ntpath

# test configs
tests = []

# solveLinearSystem
tests.append("./SolveMatSys/SolveMatSysRunTime")

# application testcases
tests.append("./2D_FV_SWE")
tests.append("./NSGTestcase/3D_FV_Stokes_fromL4")
tests.append("./swe_o0_rk2_l4_noPloy_uniform")
tests.append("./swe_o1_rk2_l4_noPloy_uniform")
tests.append("./swe_o0_rk2_l3_supercritical_b1_f28")
tests.append("./NSGTestcase/2D_FV_Poisson_fromL4")
tests.append("./NSGTestcase/2D_FD_Poisson_fromL4")#
tests.append("./NSGTestcase/3D_FV_Poisson_fromL4")
tests.append("./NSGTestcase/2D_FD_Stokes_fromL4")
tests.append("./NSGTestcase/3D_FD_Stokes_fromL4")
tests.append("./NSGTestcase/3D_FV_NavierStokes_localNewton")
tests.append("./NSGTestcase/3D_FV_NavierStokes_localPicard")
tests.append("./NSGTestcase/2D_FD_OptFlow_fromL4")
tests.append("./NSGTestcase/2D_FD_OptFlow_fromL4_Vec")

# compiletime inversion

tests.append("./invert/CompileTime/LU")
tests.append("./invert/CompileTime/BlockDiagonal")
tests.append("./invert/CompileTime/Cofactors")
tests.append("./invert/CompileTime/Diagonal")
tests.append("./invert/CompileTime/GaussJordan")
tests.append("./invert/CompileTime/Schur")
tests.append("./invert/CompileTime/SchurWithHelpers")
tests.append("./invert/CompileTime/smallMatrices")
tests.append("./invert/CompileTime/ctComparison")
 
# runtime inversion
tests.append("./invert/Runtime/Schur")
tests.append("./invert/Runtime/LU")
tests.append("./invert/Runtime/Diagonal")
tests.append("./invert/Runtime/Blockdiagonal")
tests.append("./invert/Runtime/SchurLargeMatrix")
tests.append("./invert/Runtime/SmallMatrix")

# misc functions
tests.append("./resolvingMatrixFunctions/slicing")
tests.append("./resolvingMatrixFunctions/chaines")
tests.append("./resolvingMatrixFunctions/cross")
tests.append("./resolvingMatrixFunctions/determinant")
tests.append("./resolvingMatrixFunctions/dot")
tests.append("./resolvingMatrixFunctions/trace")
tests.append("./resolvingMatrixFunctions/transpose")

# arithmetic operators
tests.append("./resolvingMatrixOperators")

# mat access
tests.append("./resolvingMatrixAccesses")

# determine structure
tests.append("./determineMatrixStructures")

# field declarations with matrix structure information
tests.append("./shapeFromField")

# matrix field accesses
tests.append("./matrixFields")



def main():
    generator_path : str#
    output_on : bool
    if(len(sys.argv) >= 3):
        if(sys.argv[1] == "reference"):
            generator_path =  "../../../ref2/exastencils/classes/artifacts/Compiler_jar/Compiler.jar"
            print("reference")
        elif(str(sys.argv[1]) == "default"):
            generator_path =  "../../classes/artifacts/Compiler_jar/Compiler.jar"
            print("default")
        if(sys.argv[2] == "true"): 
            output_on = 1
        else:
            output_on = 0
    else:
        print("not enough arguments, exiting")
        exit(-1)

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


        # generate
        print("generating " + problem_name)
        genpath : str
        if(sys.argv[1] == "reference"):
            genpath = f"{test}/Debug/gen_{problem_name}_refc.txt"
        else:
            genpath = f"{test}/Debug/gen_{problem_name}.txt"
        
        if(output_on):
            out = subprocess.run(['java', '-cp', generator_path, 'Main',  settings_path, knowledge_path])
            if(out.returncode != 0): 
                print(problem_name + " failed while generating\n")
                continue
        else:
            genfile = open(genpath,'w+') 
            out = subprocess.run(['java', '-cp', generator_path, 'Main',  settings_path, knowledge_path], stdout=genfile, stderr=genfile)
            if(out.returncode != 0): 
                print(problem_name + " failed while generating\n")
                continue
        

        # run makefile
        print("compiling " + problem_name)
        compath : str
        if(sys.argv[1] == "reference"):
            compath = f"{test}/Debug/com_{problem_name}_refc.txt"
        else:
            compath = f"{test}/Debug/com_{problem_name}.txt"
        
        if(output_on):
            out = subprocess.run(['make', '-B'],cwd=f"{test}/output/")
            if(out.returncode != 0): 
                print(problem_name + " failed while compiling\n")
                continue
        else:
            compilefile = open(compath,'w+') 
            out = subprocess.run(['make', '-B'],cwd=f"{test}/output/", stdout=compilefile, stderr=compilefile)
            if(out.returncode != 0): 
                print(problem_name + " failed while compiling\n")
                continue
        
        # run application
        print("running " + problem_name + "\n")
        runpath : str
        if(sys.argv[1] == "reference"):
            runpath = f"{test}/Debug/run_{problem_name}_refc.txt"
        else:
            runpath = f"{test}/Debug/run_{problem_name}.txt"
        
        if(output_on):
            out = subprocess.run(['./exastencils'], cwd=f"{test}/output/")
            if(out.returncode != 0): 
                print(problem_name + " failed while running\n")
                continue 
        else:
            runfile = open(runpath,'w+')   
            out = subprocess.run(['./exastencils'], cwd=f"{test}/output/", stdout=runfile, stderr=runfile)
            if(out.returncode != 0): 
                print(problem_name + " failed while running\n")
                continue    
            else:
                print("ran successful\n")

     
        # compare outputs with reference outputs
        if(len(sys.argv) == 3 and sys.argv[2] == "compare"):
            print("comparing " + problem_name + " with reference\n")
            default_comppath = f"{test}/Debug/run_{problem_name}.txt"
            ref_comppath = f"{test}/Debug/run_{problem_name}_refc.txt"
            #compfile = open(f"{test}/Debug/compare_{problem_name}.txt",'w+')   
            #out = subprocess.run(['diff', ref_comppath, default_comppath], stdout=compfile, stderr=compfile)
            out = subprocess.run(['diff', ref_comppath, default_comppath])
            
            if(out.returncode != 0): 
                print(problem_name + " failed while comparing\n")
                continue    

if __name__ == "__main__":
    main()