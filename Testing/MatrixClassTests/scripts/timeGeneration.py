import timeit
import sys
import os
import subprocess
from subprocess import DEVNULL, STDOUT, check_call
import ntpath

# test configs
tests = ["./NSGTestcase/2D_FD_Stokes_fromL4"]
tests.append("./NSGTestcase/3D_FV_NavierStokes_localNewton")
tests.append("./NSGTestcase/3D_FV_NavierStokes_localPicard")

generator_path_new = "../../out/artifacts/Compiler_jar/Compiler.jar"
generator_path_old = "../../../refc/exastencils/classes/artifacts/Compiler_jar/Compiler.jar"

def main():
    results = open("timeGenerationResults.txt","w")
    results.write("testcase    |  time new  | time old\n")
    for test in tests:
        problem_name = ntpath.basename(test)
        settings_path = f'{test}/{problem_name}.settings'
        knowledge_path = f'{test}/{problem_name}.knowledge'
        print("pname: " + problem_name)
        timeNewConfig = 0
        timeOldConfig = 0
        N = 10
        print("generating " + problem_name + " " + str(N) + " time(s) with new configuartion ...")
        for i in range (0,N):
            tic=timeit.default_timer()
            #subprocess.run(['java', '-cp', generator_path_new, 'Main',  settings_path, knowledge_path],stdout=subprocess.STDOUT, stderr=subprocess.STDOUT)
            os.system("java -cp " + generator_path_new + " Main " + settings_path + " " + knowledge_path)
            toc=timeit.default_timer()
            timeNewConfig = timeNewConfig + toc - tic #elapsed time in seconds
            print("\n\n\ngenerating " + problem_name + " " + str(N) + " time(s) with old configuartion ...")
            tic=timeit.default_timer()
            #subprocess.run(['java', '-cp', generator_path_old, 'Main',  settings_path, knowledge_path],stdout=subprocess.STDOUT, stderr=subprocess.STDOUT)
            os.system("java -cp " + generator_path_old + " Main " + settings_path + " " + knowledge_path)
            toc=timeit.default_timer()
            timeOldConfig = timeOldConfig + toc - tic #elapsed time in seconds
            if(i == 0):
                print("\n\n\naestimated runtime for 2x" + str(N) + " executions: roughly " + str((2*N/60)*(timeNewConfig + timeOldConfig)/2) + " minutes\n\n\n")

        timeOldConfig = timeOldConfig / N
        timeNewConfig = timeNewConfig / N
        results.write(test + "       " + str(timeNewConfig) + "   " + str(timeOldConfig))

if __name__ == "__main__":
    main()
