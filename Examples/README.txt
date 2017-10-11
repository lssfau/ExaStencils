
Requires a valid Compiler.jar located in /Compiler/Compiler.jar

Depending on your architecture and preference you can build it using ant (call ant in the Compiler folder) or using IntelliJ.
For the latter:
  File -> Project Structure -> Project Settings -> Artifacts -> Click green plus sign -> Jar -> From modules with dependencies -> Module: Compiler; Main Class: Main
  Output Directory -> $path_to_your_git$\Compiler
  Save
  Build -> Build Artifacts -> Compiler.jar -> build

(optional) adapt platform file used in Examples/generateExamples.sh
(optional) deactivate unrequired configurations in Examples/examples.sh

afterwards simply run
cd Examples
./generateExamples.sh
./compileExamples.sh
./runExamples.sh
