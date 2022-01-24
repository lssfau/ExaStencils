<!---



A nicely formatted online version of this README should be available here: https://hackmd.io/s/SJof5RTnG
The source is available here: https://hackmd.io/_HIHDpfEQ5-JQKSCucw2Sg?both



--->



# ![ExaStencils](Documentation/logos/ExaStencilsLogo.png)

The [ExaStencils](https://www.exastencils.fau.de/) code generation framework processes input in its own multi-layered domain-specific language (DSL) to emit highly optimized and massively parallel geometric multigrid solvers for (block-)structured grids.

This repository holds the current release version of ExaStencils.

## Setup

For building the generator, a JDK is required. We recommend using version 11.

### IDE Support

We recommend using IntelliJ IDEA (the community edition is fine). Downloads can be found [here](https://www.jetbrains.com/idea/download/).

*Using an IDE is not required.* Instructions on how to do things without it can be found [here](#sbt).

#### Setting up the IDE for Coding

First, check that the Scala plugin is installed: File -> Settings -> Plugins

If you plan to commit code please use our code style. It is located in /Documentation/IntelliJ/ExaStencils.xml and can be imported like this:
*  File -> Settings -> Editor -> Code Style -> Scala -> Import Scheme (click small gear) -> IntelliJ IDEA code style XML
*  locate /Documentation/IntelliJ/ExaStencils.xml
*  ok

If you are used to another IDE, e.g. Eclipse, setting the keymap to an according style may be helpful:
*  File -> Settings -> Keymap -> Eclipse

#### Compiling the Generator

* If IntelliJ is opened the first time:
  * Open
  * select path in which you checked out the git repository -> ok
  * ok
* If already in the IDE:
  * File -> New -> project from existing sources
  * select path in which you checked out the git repository -> ok
  * import project from external model -> sbt -> next
  * Finish
* if 'Add Files to Git' dialogue opens -> don't add anything (press cancel)
* check that everything works: Build -> Build Project
* **note** in some configurations it might be necessary to add the Scala SDK under Project Structure -> Global Libraries. Otherwise no files will be compiled (build still succeeds).

#### Creating a JAR

Inside the IDE do the following to create a task to assemble the jar:
* Run -> Edit configurations
  * green plus (upper left corner) -> sbt Task
  * fill name  : assembly
  * fill tasks : assembly
  * Ok

The first step has to be done only once. Afterwards running the task is sufficient to assemble the jar(s).
* Run -> Run 'assembly' (if this is not available use Run -> Run... and select assembly manually)

**alternatively**, adding artifacts in IntelliJ is possible as well and often faster:
* File -> Project Structure -> Project Settings -> Artifacts -> Click green plus sign -> Jar -> From modules with dependencies
    * Module: Compiler
    * Main Class: Main
    * ok
* Output Directory -> \$path_to_your_git\$\Compiler
* ok

The jar can then be created using 
* Build -> Build Artifacts -> Compiler.jar -> build

Artifacts can be added for the other sub-projects in a similar fashion.

### sbt

For users that don't want to use an IDE or want to compile on the command line, sbt is required.
*If you are using Windows we recommend using Ubuntu Shell which is part of the [Windows Subsystem for Linux (WSL)](https://docs.microsoft.com/en-us/windows/wsl/install-win10).*

The first step is installing sbt -- a guide can be found [here](https://www.scala-sbt.org/1.0/docs/Installing-sbt-on-Linux.html)

Next, open a shell and locate the folder the git repository has been checked out to.
Compilation is done via typing

    sbt compile

To assemble a jar the following command is available

    sbt assembly

### CImg support

For CImg support, the corresponding CImg.h header file needs to be downloaded such that it can be used as ressource by our generator. This can be done in three different ways:
* for users using sbt on the command line: ```sbt downloadCImg```
* for IntelliJ users:
    * Run -> Run -> 0: Edit Configurations -> + -> sbtTask
    * Name: downloadCImg; Tasks: downloadCImg; Run
* for all users: directly download the required file from [here](https://github.com/dtschump/CImg) and place it in Compiler/res/
Updating the file works the same way.
  
### VisIt support

In order to make use of VisIt's in-situ visualization capabilities, it is recommended to build VisIt from scratch. The sources can be downloaded from [GitHub](https://github.com/visit-dav/visit/releases/)

1. Download the VisIt build script (version 3.1.4)

```
wget https://github.com/visit-dav/visit/releases/download/v3.1.4/build_visit3_1_4
chmod +x build_visit3_1_4
```

2. Build 3rd party libraries

> The available libraries and options for building VisIt can be displayed via `./build_visit3_1_4 --help`. \
> You can print the relevant environment variables via `./build_visit3_1_4 --print-vars`. \
> At first, we only build the missing 3rd party packages. You can find one of the faster (and parallel) builds below. \
> Still, this can still take ~4 hours. \
> The building process can be completed faster via the `--makeflags -j4` option. \
> Note that VisIt is not built yet (`--no-visit` option). \
> Remember to replace `<INSTALL_DIR_PATH>` with, e.g., `/usr/local/visit`.

```
env PAR_COMPILER=mpicc ./build_visit3_1_4 --no-visit --mesagl --openssl --zlib --parallel --no-thirdparty --cmake --vtk --qt --qwt --python --silo --hdf5 --szip --llvm --prefix <INSTALL_DIR_PATH>
```

During this process, you might need to install one of the following dependencies:

```
sudo apt install mesa-common-dev
sudo apt-get install libxext-dev
sudo apt-get install libdrm-dev libxxf86vm-dev libxt-dev xutils-dev flex bison xcb libx11-xcb-dev libxcb-glx0 libxcb-glx0-dev xorg-dev libxcb-dri2-0-dev libxcb-xfixes0-dev
```

After that, the third-party cmake settings can be found in `./<hostname>.cmake`

3. Download VisIt

```
wget https://github.com/visit-dav/visit/releases/download/v3.1.4/visit3.1.4.tar.gz
tar -xzf visit3.1.4.tar.gz
```

4. Configure VisIt

```
cp <hostname>.cmake visit3.1.4/src/config-site/
cd visit3.1.4/src
../../cmake-3.9.3/bin/ccmake .
```

> then press 'c' to configure\
> check `VISIT_PARALLEL=ON`\
> press 'c' until 'g' is available\
> press 'g' to generate Makefile

5. Build and install VisIt

Finally, we can compile, package and install VisIt

```
# build
make -j 8

# package and install (needs sudo privileges)
# e.g. ARCH = linux-x86_64
make package
wget https://github.com/visit-dav/visit/releases/download/v3.1.4/visit-install3_1_4
chmod +x visit-install3_1_4
visit-install3_1_4 3.1.4 <ARCH> <INSTALL_DIR_PATH>
```

6. Set environment variables

```
export VISIT_HOME='<INSTALL_DIR_PATH>'
export PATH=$VISIT_HOME/bin:$VISIT_HOME/current/bin:$PATH
export SIMV2DIR=$VISIT_HOME/current/linux-x86_64/libsim/V2
export LD_LIBRARY_PATH=$SIMV2DIR/lib:$VISIT_HOME/current/linux-x86_64/lib/mesagl:$VISIT_HOME/current/linux-x86_64/lib:"${LD_LIBRARY_PATH}"
```

7. Run VisIt

```
# serial
visit
# parallel
visit -np <num processors>
```

For more input options, call `visit --help` or `visit -fullhelp`


More details for building or installing VisIt can be found here:
* https://trac.version.fz-juelich.de/vis/wiki/VisIt/build_x86
* https://www.visitusers.org/index.php?title=Build_visit_overview
* http://www.visitusers.org/index.php?title=ParallelPorting#Compiling_VisIt_to_run_in_parallel

## First Steps

### Examples

We recommend having a look at the examples located in /Examples.
To generate the examples, a valid Compiler.jar has to be located in /Compiler/Compiler.jar. You can create one using any of the guides above.

(optional) It might be necessary to adapt the platform file used in /Examples/generateExamples.sh
(optional) By default, all examples are generated. Unrequired configurations can be deactivated in /Examples/examples.sh

If Linux is used (or the Ubuntu Shell for Windows), generating, compiling and executing the examples is scripted. The following commands can simply be executed in the folder the git repository has been checked out to:

    cd Examples
    ./generateExamples.sh
    ./compileExamples.sh
    ./runExamples.sh

### Generating User Applications

Generating single configurations is possible by executing the generator (Compiler project). The following command line arguments have to be provided in this exact order:
* Settings
* Knowledge
* Platform

## Documentation

A detailed documentation is currently work in progress.

## Contributing 

We always welcome and appreciate contributions to ExaStencils.
Before contributing, please refer to our [contributing guidelines](https://github.com/lssfau/ExaStencils/blob/master/CONTRIBUTING.md) first.

## Authors

The main authors are Sebastian Kuckuk, Christian Schmitt and Stefan Kronawitter. We are thankful for the work of all [contributors](https://github.com/lssfau/ExaStencils/blob/master/AUTHORS.txt).

## License

The ExaStencils code generation framework is licensed under [GPLv3](https://github.com/lssfau/ExaStencils/blob/master/COPYING.txt).

## Dependencies

This project depends on the [cloning](https://github.com/kostaskougios/cloning) and [objenesis](http://objenesis.org/) libraries which are both licensed under the Apache license version 2.0. When the CImg visualization module is used, it depends on the [CImg](https://framagit.org/dtschump/CImg) header licensed under the CeCILL-C licence.

This project depends on and deploys the [isl](https://repo.or.cz/w/isl.git) library with added [scala bindings](https://xxx.de) as well as the [Chernikova](https://xxx.de) library. All of them are licensed under the MIT license.

All dependencies are managed automatically via sbt, with the exception of CImg which has to be taken care of [manually](#CImg-support).
