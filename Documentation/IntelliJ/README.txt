
Project setup:
	import project from eclipse
	troubleshoot if necessary:
		check that scala sdk is part of both modules (file -> project structure -> modules -> { Compiler; CompilerMacros } -> dependencies) - otherwise scala files will not be compiled!
		if missing: add ( green plus ) -> 2 library -> scala sdk
		check that the CompilerMacros module is a dependency of Compiler
		check that src folders are marked as source folders (file -> project structure -> modules -> { Compiler; CompilerMacros } -> sources)
		rebuild

useful:
	Keymap -> Eclipse
