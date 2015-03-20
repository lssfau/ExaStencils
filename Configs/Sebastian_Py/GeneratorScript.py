import sys
import getopt
import os
import time
from time import gmtime, strftime
import subprocess
from collections import defaultdict

import Functions


basePath = "C:\\Users\\sisekuck\\Documents\\Visual Studio 2010\\Projects\\ScalaExaStencil\\Heap\\generate_hannah"
targetPlatformBasePath = "/homea/her18/her182/sisc/test"


def write_settings(path, config):
    if not os.path.exists(path):
        os.makedirs(path)
    settings_file = open(path + "/" + config.baseName + ".settings", 'w+')

    settings_file.write("user						= \"Sebastian\"\n")
    settings_file.write("outputPath					= \"" + basePath + "/" + config.baseName + "/\"\n")
    settings_file.write("basePathPrefix				= \".\"\n")
    # settings_file.write("failOnConstraint			= true\n")
    # settings_file.write("cancelIfOutFolderExists		= true\n")
    settings_file.write("produceHtmlLog				= true\n")

    settings_file.close()


def write_knowledge(path, config):
    if not os.path.exists(path):
        os.makedirs(path)
    file = open(path + "/" + config.baseName + ".knowledge", 'w+')

    file.write("// constant parameters\n")
    for param in sorted(config.constParameters):
        file.write("%s = %s\n" % (param, config.constParameters[param]))
    file.write("\n")

    file.write("// derived parameters\n")
    for param in sorted(config.derivedParameters):
        file.write("%s = %s\n" % (param, config.derivedParameters[param]))
    file.write("\n")

    file.write("// ranged parameters\n")
    for param in sorted(config.chosenRangedParameters):
        if "nameModifier" != param:
            file.write("%s = %s\n" % (param, config.chosenRangedParameters[param]))
    file.write("\n")

    file.write("// listed parameters\n")
    for param in sorted(config.chosenListedParameters):
        if "nameModifier" != param:
            file.write("%s = %s\n" % (param, config.chosenListedParameters[param]))
    file.write("\n")

    file.close()


def generate_files(configurations):
    for config in configurations:
        print("Writing files for configuration " + config.baseName)
        out_path = basePath + "/" + config.baseName
        write_settings(out_path, config)
        write_knowledge(out_path, config)


def generate_solvers(configurations):
    cwd = os.getcwd()
    os.chdir("C:/Users/sisekuck/Documents/Visual Studio 2010/Projects/ScalaExaStencil")

    try:
        for config in configurations:
            print("Generating code for configuration " + config.baseName + "...")
            if os.path.isfile(basePath + "/" + config.baseName + "/Makefile"):
                print("... already existent")
            else:
                settings_file = basePath + "/" + config.baseName + "/" + config.baseName + ".settings"
                knowledge_file = basePath + "/" + config.baseName + "/" + config.baseName + ".knowledge"
                command = ["java.exe", "-Xmx2G", "-Xms2G", "-cp",
                           "C:\\Eclipse\\plugins\\org.scala-lang.scala-library_2.11.2.v20140721-095018-73fb460c1c.jar;" +
                           "C:\\Eclipse\\plugins\\org.scala-lang.scala-reflect_2.11.2.v20140721-095018-73fb460c1c.jar;" +
                           ".\\Compiler\\bin;" +
                           ".\\CompilerMacros\\CompilerMacros\\bin;" +
                           ".\\Compiler\\lib\\*",
                           "Main", settings_file, knowledge_file]

                with open(os.devnull, "w") as nowhere:
                    subprocess.call(command, stdout=nowhere)
                print("... done")

    finally:
        os.chdir(cwd)


def generate_compile_script(path, filename, configurations):
    if not os.path.exists(path):
        os.makedirs(path)
    script_file = open(path + "/" + filename, 'w+')

    script_file.write("#!/bin/bash\n")

    for config in configurations:
        script_file.write("cd " + targetPlatformBasePath + "/" + config.baseName + "\n")
        script_file.write("make clean\n")
        script_file.write("time make -j\n")

    script_file.close()


def generate_copy_script(path, filename, configurations):
    if not os.path.exists(path):
        os.makedirs(path)
    script_file = open(path + "/" + filename, 'w+')

    script_file.write("#!/bin/bash\n")
    script_file.write("\n")

    script_file.write("mkdir $WORK/ExaTemp # make sure parent temp folder exists\n")
    script_file.write("\n")

    for config in configurations:
        script_file.write("mkdir $WORK/ExaTemp/" + config.baseName + " # make sure temp folder exists\n")
        script_file.write("cp " + targetPlatformBasePath + "/" + config.baseName +
                          "/exastencils $WORK/ExaTemp/" + config.baseName + "/exastencils" +
                          " # copy binary to temp folder\n")
        script_file.write("\n")

    script_file.close()


def generate_run_script(path, filename, configurations, copy_files):
    if not os.path.exists(path):
        os.makedirs(path)

    jobs = defaultdict(lambda: defaultdict(list))

    for config in configurations:
        jobs[config.get_num_nodes()][config.get_rpn()].append(config)

    for job_size in jobs:
        print("Writing jobscript " + filename + "_" + str(job_size))
        script_file = open(path + "/" + filename + "_" + str(job_size), 'w+')

        script_file.write("#@ shell = /bin/bash\n")
        script_file.write("#@ job_name = GENERATED_" + str(job_size) + "\n")
        script_file.write("#@ error = $(job_name).$(jobid).out\n")
        script_file.write("#@ output = $(job_name).$(jobid).out\n")
        script_file.write("#@ environment = COPY_ALL\n")
        script_file.write("#@ notification = always\n")
        script_file.write("#@ notify_user = sebastian.kuckuk@fau.de\n")
        script_file.write("#@ job_type = bluegene\n")
        script_file.write("#@ bg_size = " + str(job_size) + "\n")
        script_file.write("#@ bg_connectivity = TORUS\n")
        script_file.write("#@ wall_clock_limit = 00:30:00\n")  # 30 minutes is the hard limit for smallest queues
        script_file.write("#@ queue\n")
        script_file.write("\n")

        if copy_files:
            script_file.write("#--------------------------------\n")
            script_file.write("# create temp dirs and copy executables\n")
            script_file.write("#--------------------------------\n")
            script_file.write("\n")

            script_file.write("mkdir $WORK/ExaTemp # make sure parent temp folder exists\n")

            for ranks_per_node in sorted(jobs[job_size]):
                for config in sorted(jobs[job_size][ranks_per_node]):
                    script_file.write("mkdir $WORK/ExaTemp/" + config.baseName + " # make sure temp folder exists\n")
                    script_file.write("cp " + targetPlatformBasePath + "/" + config.baseName +
                                      "/exastencils $WORK/ExaTemp/" + config.baseName + "/exastencils" +
                                      " # copy binary to temp folder\n")
                    script_file.write("\n")

        script_file.write("#--------------------------------\n")
        script_file.write("# run executables\n")
        script_file.write("#--------------------------------\n")
        script_file.write("\n")

        for ranks_per_node in sorted(jobs[job_size]):
            for config in sorted(jobs[job_size][ranks_per_node]):
                script_file.write("echo \"Going to config " + config.baseName + "\"\n")
                script_file.write("cd $WORK/ExaTemp/" + config.baseName + " # switch to temp folder\n")
                script_file.write("if [ ! -f ./timings.csv ]; then\n")
                script_file.write("\texport OMP_NUM_THREADS=" + str(config.get_num_omp()) + "\n")
                script_file.write("\ttime runjob"
                                  + " --ranks-per-node " + str(ranks_per_node)
                                  + " --np " + str(config.get_num_mpi())
                                  + " --exp-env OMP_NUM_THREADS"
                                  + " : ./exastencils\n")
                script_file.write("fi\n")
                script_file.write("\n")
            script_file.write("\n")

        script_file.close()


def generate_toolchain_script(path, filename, configurations, compile_script, run_script):
    if not os.path.exists(path):
        os.makedirs(path)
    script_file = open(path + "/" + filename, 'w+')

    script_file.write("#!/bin/bash\n")

    script_file.write("chmod 700 " + compile_script + "\n")

    jobs = defaultdict(list)
    for config in configurations:
        jobs[config.get_num_nodes()].append(config)

    script_file.write("time " + compile_script + " && "
                      + " && ".join(map(lambda k: "llsubmit " + run_script + "_" + str(k), jobs))
                      + " && watch llq -u her182"
                      + "\n")

    script_file.close()


def main(argv):
    project_filename = ''
    config_list_filename = ''
    opts, args = getopt.getopt(argv, "p:c:")
    for opt, arg in opts:
        if opt == '-p':
            project_filename = arg
        elif opt == '-c':
            config_list_filename = arg
        else:
            print('Unknown option:' + opt + arg)
            sys.exit()

    if '' == project_filename:
        print('No project configuration provided.')
        return

    start_time = gmtime()
    print("Starting at: " + strftime("%Y-%m-%d %H:%M:%S", start_time))

    print("Loading project configuration")
    parameters = Functions.load_config_class(project_filename)

    # get configs
    configs = Functions.init_configurations(config_list_filename, parameters.Configuration)

    print("Setting up files")
    generate_files(configs)
    print("Generating code for solvers")
    generate_solvers(configs)
    print("Setting up compile script")
    generate_compile_script(basePath, "compileAll", configs)
    print("Setting up copy scripts")
    generate_copy_script(basePath, "copyToWork", configs)
    print("Setting up job scripts")
    generate_run_script(basePath, "runJuQueen", configs, True)
    print("Setting up toolchain scripts")
    generate_toolchain_script(basePath, "doStuff", configs, "compileAll", "runJuQueen")

    print("\n\nFinished at: " + strftime("%Y-%m-%d %H:%M:%S", gmtime()))
    delta_time = time.mktime(gmtime()) - time.mktime(start_time)
    print("Total time required: " + str(delta_time) + " s.")


main(sys.argv[1:])