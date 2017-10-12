
IDE setup:
download and install IntelliJ IDEA (community edition is fine) https://www.jetbrains.com/idea/download/

Project setup:
	open IntelliJ
  import project from eclipse
  import ExaStencils.xml as code style: settings -> editor -> code style -> scala -> import scheme (click small gear)
	troubleshoot if necessary:
		check that scala sdk is part of both modules (file -> project structure -> modules -> { Compiler; CompilerMacros } -> dependencies) - otherwise scala files will not be compiled!
		if missing: add ( green plus ) -> 2 library -> scala sdk
		check that the CompilerMacros module is a dependency of Compiler
		check that src folders are marked as source folders (file -> project structure -> modules -> { Compiler; CompilerMacros } -> sources)
		rebuild

Alternative project setup:
	copy both Compiler.iml and CompilerMacros.iml to correspronding directories and import them in IntelliJ (starting with CompilerMacros)
  import ExaStencils.xml as code style: settings -> editor -> code style -> scala -> import scheme (click small gear)

useful:
	Keymap -> Eclipse
