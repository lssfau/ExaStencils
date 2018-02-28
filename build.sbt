name := "ScalaExaStencils"

version := "1.0"

scalaVersion := "2.12.4"

lazy val CompilerMacros = (project in file("CompilerMacros"))

lazy val Compiler = (project in file("Compiler")).dependsOn(CompilerMacros).
  settings(
    assemblyOutputPath in assembly := file("Compiler/compiler.jar"),
  )
