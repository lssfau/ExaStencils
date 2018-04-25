<!---



A nicely formatted online version of this README should be available here: https://hackmd.io/s/SJof5RTnG
The source is available here: https://hackmd.io/_HIHDpfEQ5-JQKSCucw2Sg?both



--->



# The ExaStencils Code Generator

## IDE Support

We recommend using IntelliJ IDEA (the community edition is fine).
Downloads can be found [here](https://www.jetbrains.com/idea/download/).

*Using an IDE is not required.* Instructions on how to do things without it can be found [here](#sbt).

### Setting up the IDE for Coding

If you plan to commit code please use our code style. It is located in /Documentation/IntelliJ/ExaStencils.xml and can be imported like this:
*  File -> Settings -> Editor -> Code Style -> Scala -> Import Scheme (click small gear) -> IntelliJ IDEA code style XML
*  locate /Documentation/IntelliJ/ExaStencils.xml
*  ok

You should also have a look at the coding guidelines found in /Documentation/CodingGuidelines.scala

If you are used to Eclipse, setting the keymap to Eclipse style may be helpful:
*  File -> Settings -> Keymap -> Eclipse

### Compiling the Generator

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

*Alternatively*, provided project files can be used directly (not recommended):
* copy Documentation/IntelliJ/Compiler.iml to /Compiler
* copy Documentation/IntelliJ/CompilerMacros.iml to /CompilerMacros
* Import both in IntelliJ starting with CompilerMacros

### Creating a JAR

Inside the IDE do the following to create a task to assemble the jar:
* Run -> Edit configurations
  * green plus (upper left corner) -> sbt Task
  * fill name  : assembly
  * fill tasks : assembly
  * Ok

The first step has to be done only once. Afterwards running the task is sufficient to assemble the jar(s).
* Run -> Run 'assembly' (if this is not available use Run -> Run... and select assembly manually)

**alternatively**, adding artifacts in IntelliJ is possible as well:
* File -> Project Structure -> Project Settings -> Artifacts -> Click green plus sign -> Jar -> From modules with dependencies
    * Module: Compiler
    * Main Class: Main
    * ok
* Output Directory -> \$path_to_your_git\$\Compiler
* ok

The jar can then be created using 
* Build -> Build Artifacts -> Compiler.jar -> build

Artifacts can be added for the other sub-projects in a similar fashion.

## sbt

For users that don't want to use an IDE or want to compile on the command line, sbt is required.
*If you are using Windows we recommend using Ubuntu Shell which is part of the [Windows Subsystem for Linux (WSL)](https://docs.microsoft.com/en-us/windows/wsl/install-win10).*

The first step is installing sbt -- a guide can be found [here](https://www.scala-sbt.org/1.0/docs/Installing-sbt-on-Linux.html)

Next, open a shell and locate the folder the git repository has been checked out to.
Compilation is done via typing
* sbt compile

To assemble a jar the following command is available
* sbt assembly

## First Steps

### Examples

We recommend having a look at the examples located in /Examples.
To generate the examples, a valid Compiler.jar has to be located in /Compiler/Compiler.jar. You can create one using any of the guides above.

(optional) It might be necessary to adapt the platform file used in /Examples/generateExamples.sh
(optional) By default, all examples are generated. Unrequired configurations can be deactivated in /Examples/examples.sh

If Linux is used (or the Ubuntu Shell for Windows), generating, compiling and executing the examples is scripted. The following commands can simply be executed in the folder the git repository has been checked out to:
* cd Examples
* ./generateExamples.sh
* ./compileExamples.sh
* ./runExamples.sh

### Generating User Applications

Generating single configurations is possible by executing the generator (Compiler project). The following command line arguments have to be provided in this exact order:
* Settings
* Knowledge
* Platform
