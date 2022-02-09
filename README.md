<!---



A nicely formatted online version of this README should be available here: https://hackmd.io/s/SJof5RTnG
The source is available here: https://hackmd.io/_HIHDpfEQ5-JQKSCucw2Sg?both



--->



# ![ExaStencils](Documentation/logos/ExaStencilsLogo.png)

The ExaStencils code generation framework processes input in its own multi-layered domain-specific language (DSL) to emit highly optimized and massively parallel geometric multigrid solvers for (block-)structured grids.

This repository holds the current version of ExaStencils.

Project information and documentation are available via the official homepage: https://www.exastencils.fau.de/. 

The source code is accessible from GitLab (https://i10git.cs.fau.de/exastencils/release/) and GitHub (https://github.com/lssfau/ExaStencils/).

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

### Parallel I/O Libraries

Currently, following libraries for parallel I/O are supported in ExaStencils:
* MPI I/O
* HDF-5
* PNetCDF (+ ExodusII)
* SionLib

MPI I/O is part of MPI-2 standard and is already installed on most HPC systems. However, the other I/O libraries need to be installed first. Installation guides for each dependency are listed below.

### HDF5
To build and install HDF5 version 1.12.0, the following lines can be used:
```
wget https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.12/hdf5-1.12.0/src/hdf5-1.12.0.tar.gz
tar xvf hdf5-1.12.0.tar.gz
cd hdf5-1.12.0
CC=/path/to/bin/mpicc ./configure --enable-parallel --prefix=$HOME/hdf5

make
make install
```
Other versions can be downloaded [here](https://portal.hdfgroup.org/display/support/Downloads) and further installation guidelines can be found [here](https://support.hdfgroup.org/ftp/HDF5/current18/src/unpacked/release_docs/).

### PnetCDF

To build and install PnetCDF version 1.12.1, the following lines can be used:
```
wget http://cucis.ece.northwestern.edu/projects/PnetCDF/Release/pnetcdf-1.12.1.tar.gz
tar xvf pnetcdf-1.12.1.tar.gz
cd pnetcdf-1.12.1
CC=/path/to/bin/mpicc ./configure --prefix=$HOME/PnetCDF # setting MPICC or using --with-mpi option didnt work

make -j 8
make install
```
Other versions can be downloaded [here](http://cucis.ece.northwestern.edu/projects/PnetCDF/download.html) and further installation guidelines can be found [here](https://svn.mcs.anl.gov/repos/parallel-netcdf/trunk/INSTALL).

### ExodusII

To build and install the current version of the ExodusII library, the following lines can be used:
```
git clone https://github.com/gsjaardema/seacas.git
cd seacas && export ACCESS=`pwd`

MPI=ON ./install-tpl.sh
cd $ACCESS
mkdir build
cd build

INSTALL_PATH=$HOME/seacas FORTRAN=OFF ../cmake-exodus
make && make install

# in case of error: installing following dependencies might help
sudo apt-get install libtool m4 automake
sudo apt-get install libopenmpi-dev
```
Further installation guides and older versions can be found [here](https://github.com/gsjaardema/seacas).

### SIONlib

To build and install SIONlib version 1.7.6, following lines can be used:
```
curl "http://apps.fz-juelich.de/jsc/sionlib/download.php?version=1.7.6" -o sionlib.tar.gz
tar xvf sionlib.tar.gz
cd sionlib
./configure --prefix=$HOME/sionlib --disable-fortran --mpi=openmpi
cd build-linux-gomp-openmpi

make
make install
```
Other versions can be downloaded [here](https://www.fz-juelich.de/ias/jsc/EN/Expertise/Support/Software/SIONlib/sionlib-download_node.html;jsessionid=8FC564790A3592233CB1ECA4D4F981FF) and further installation guidelines can be found [here](https://apps.fz-juelich.de/jsc/sionlib/docu/1.7.6/installation_page.html).

### Environment

For the generated Makefiles, following environment variables need to be defined depending on the utilized I/O libraries: `HDF5_HOME`, `PNETCDF_HOME`, `SION_HOME` and `EXODUS_HOME`
```
# define home directories for new dependencies
BASEDIR=$HOME
HDF5_HOME=$BASEDIR/hdf5 ; export HDF5_HOME
PNETCDF_HOME=$BASEDIR/PnetCDF ; export PNETCDF_HOME
SION_HOME=$BASEDIR/sionlib ; export SION_HOME
EXODUS_HOME=$BASEDIR/seacas; export EXODUS_HOME

# path
PATH=$HDF5_HOME/bin:$PATH ; export PATH
PATH=$PNETCDF_HOME/bin:$PATH ; export PATH
PATH=$SION_HOME/bin:$PATH ; export PATH
PATH=$EXODUS_HOME/bin:$PATH; export PATH

# ld_library_path
LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$HDF5_HOME/lib:$PNETCDF_HOME/lib:$EXODUS_HOME/lib ; export LD_LIBRARY_PATH
```


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
