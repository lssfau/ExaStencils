<!---



A nicely formatted online version of this README should be available here: https://hackmd.io/s/SJof5RTnG
The source is available here: https://hackmd.io/_HIHDpfEQ5-JQKSCucw2Sg?both



--->



# ![ExaStencils](Documentation/logos/ExaStencilsLogo.png)

The ExaStencils code generation framework processes input in its own multi-layered domain-specific language (DSL) to emit highly optimized and massively parallel geometric multigrid solvers for (block-)structured grids.

This repository holds the current release version of ExaStencils.

## Setup

For building the generator, a JDK is required. We recommend using version 11.

### IDE Support

We recommend using IntelliJ IDEA (the community edition is fine). Downloads can be found [here](https://www.jetbrains.com/idea/download/).

*Using an IDE is not required.* Instructions on how to do things without it can be found [here](#sbt).

#### Setting up the IDE for Coding

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
* for all users: directly download the required file from [here](https://framagit.org/dtschump/CImg) and place it in Compiler/res/
Updating the file works the same way.

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
Before contributing, please refer to our [contributing guidelines](https://i10git.cs.fau.de/exastencils/exastencils/-/blob/master/CONTRIBUTING.md) first.

## Authors

The main authors are Sebastian Kuckuk, Christian Schmitt and Stefan Kronawitter. We are thankful for the work of all [contributors](https://i10git.cs.fau.de/exastencils/exastencils/-/blob/master//AUTHORS.txt).

## License

The ExaStencils code generation framework is licensed under [GPLv3](https://i10git.cs.fau.de/exastencils/exastencils/-/blob/master//COPYING.txt).

## Dependencies

This project depends on the [cloning](https://github.com/kostaskougios/cloning) and [objenesis](http://objenesis.org/) libraries which are both licensed under the Apache license version 2.0. When the CImg visualization module is used, it depends on the [CImg](https://framagit.org/dtschump/CImg) header licensed under the CeCILL-C licence.

This project depends on and deploys the [isl](https://repo.or.cz/w/isl.git) library with added [scala bindings](https://xxx.de) as well as the [Chernikova](https://xxx.de) library. All of them are licensed under the MIT license.

All dependencies are managed automatically via sbt, with the exception of CImg which has to be taken care of [manually](#CImg-support).
