import sys
import getopt
import os
import imp
import time
from time import gmtime, strftime
import subprocess
from collections import defaultdict

basePath = "C:\\Users\\sisekuck\\Documents\\Visual Studio 2010\\Projects\\ScalaExaStencil\\Heap\\generate"
targetPlatformBasePath = "/homea/her18/her182/sisc/test"


def write_settings(path, config):
    if not os.path.exists(path):
        os.makedirs(path)
    settings_file = open(path + "/" + config.baseName + ".settings", 'w+')

    settings_file.write("user						= \"Sebastian\"\n")
    settings_file.write("outputPath					= " +
                        "\"C:/Users/sisekuck/Documents/Visual Studio 2010/Projects/ScalaExaStencil/Heap/generate/" +
                        config.baseName + "/\"\n")
    settings_file.write("basePathPrefix				= \".\"\n")
    # settings_file.write("cancelIfOutFolderExists		= true\n")

    settings_file.close()


def write_knowledge(path, config):
    if not os.path.exists(path):
        os.makedirs(path)
    file = open(path + "/" + config.baseName + ".knowledge", 'w+')

    file.write("// constant parameters\n")
    for param in config.constParameters:
        file.write("%s = %s\n" % (param[0], param[1]))
    file.write("\n")

    file.write("// ranged parameters\n")
    for param in config.chosenRangedParameters:
        file.write("%s = %s\n" % (param[0], param[1]))
    file.write("\n")

    file.write("// listed parameters\n")
    for param in config.chosenListedParameters:
        file.write("%s = %s\n" % (param[0], param[1]))
    file.write("\n")

    file.close()


def load_config(config_file):
    if not os.path.isfile(config_file):
        print("Unable to load file:" + config_file)
        return

    print("Using project configuration " + config_file)

    # var_module = os.path.abspath(config_file)
    config = imp.load_source('vars', config_file)
    print("Successfully imported " + config_file)
    return config


def generate_configurations(configuration_class):
    extended_parameters = [configuration_class()]

    for param in configuration_class.rangedParameters:
        new_parameters = []
        it = param[1]
        while it <= param[2]:
            for config in extended_parameters:
                new_config = configuration_class()
                new_config.baseName = config.baseName
                new_config.chosenRangedParameters = config.chosenRangedParameters[:]
                new_config.chosenListedParameters = config.chosenListedParameters[:]
                new_config.chosenRangedParameters.append([param[0], it])
                new_config.baseName += "_" + str(it)
                new_parameters.append(new_config)
            it = param[3](it)
        extended_parameters = new_parameters

    for param in configuration_class.listedParameters:
        new_parameters = []
        for it in param[1]:
            for config in extended_parameters:
                new_config = configuration_class()
                new_config.baseName = config.baseName
                new_config.chosenRangedParameters = config.chosenRangedParameters[:]
                new_config.chosenListedParameters = config.chosenListedParameters[:]
                new_config.chosenListedParameters.append([param[0], it])
                new_config.baseName += "_" + it.replace('"', '').strip()
                new_parameters.append(new_config)
        extended_parameters = new_parameters

    print("Found %s configurations" % len(extended_parameters))
    for config in extended_parameters:
        config.update()
    extended_parameters = [config for config in extended_parameters if config.is_valid()]
    print("After filtering, %s valid configurations remain" % len(extended_parameters))
    return extended_parameters


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
            print("Generating code for configuration " + config.baseName)
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


def generate_run_script(path, filename, configurations):
    if not os.path.exists(path):
        os.makedirs(path)

    jobs = defaultdict(list)

    for config in configurations:
        jobs[config.getNumNodes()].append(config)

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
        script_file.write("#@ wall_clock_limit = 00:30:00\n")
        script_file.write("#@ queue\n")
        script_file.write("\n")

        script_file.write("mkdir $WORK/ExaTemp # make sure temp folder exists\n")
        script_file.write("\n")

        for config in jobs[job_size]:
            script_file.write("cp " + targetPlatformBasePath + "/" + config.baseName +
                              "/exastencils $WORK/ExaTemp/exastencils_" + config.baseName +
                              " # copy binary to temp folder\n")

        script_file.write("\n")
        script_file.write("cd $WORK/ExaTemp # switch to temp folder\n")
        script_file.write("\n")

        for config in jobs[job_size]:
            script_file.write("export OMP_NUM_THREADS=" + str(config.getNumOMP()) + "\n")
            # FIXME: RPN
            script_file.write("time runjob --ranks-per-node 64 --np " + str(
                config.get_num_mpi()) + " --exp-env OMP_NUM_THREADS : ./exastencils_" + config.baseName + "\n")
            script_file.write("\n")

        script_file.close()


def main(argv):
    config_file = ''
    opts, args = getopt.getopt(argv, "f:r:g")
    for opt, arg in opts:
        if opt == '-f':
            config_file = arg
        else:
            print('Unknown option:' + opt + arg)
            sys.exit()

    if '' == config_file:
        print('No project configuration provided.')
        return

    start_time = gmtime()
    print("Starting at: " + strftime("%Y-%m-%d %H:%M:%S", start_time))

    parameters = load_config(config_file)
    print("Generating configurations")
    configs = generate_configurations(parameters.Configuration)
    print("Setting up files")
    generate_files(configs)
    print("Generating code for solvers")
    generate_solvers(configs)
    print("Setting up compile script")
    generate_compile_script(basePath, "compileAll", configs)
    print("Setting up job scripts")
    generate_run_script(basePath, "runJuQueen", configs)

    print("\n\nFinished at: " + strftime("%Y-%m-%d %H:%M:%S", gmtime()))
    delta_time = time.mktime(gmtime()) - time.mktime(start_time)
    print("Total time required: " + str(delta_time) + " s.")


main(sys.argv[1:])