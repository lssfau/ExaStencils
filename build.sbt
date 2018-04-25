name := "ScalaExaStencils"

version := "1.0"

scalaVersion := "2.12.4"

disablePlugins(sbtassembly.AssemblyPlugin)

lazy val CompilerMacros = (project in file("CompilerMacros")).disablePlugins(sbtassembly.AssemblyPlugin)

lazy val Compiler = (project in file("Compiler")).dependsOn(CompilerMacros).
  settings(
    assemblyOutputPath in assembly := file("Compiler/compiler.jar"),
  )

lazy val ConfigRunner = (project in file("ConfigRunner")).dependsOn(Compiler, CompilerMacros).
  settings(
    assemblyOutputPath in assembly := file("ConfigRunner/ConfigRunner.jar"),
  )

lazy val Meta = (project in file("Meta")).dependsOn(Compiler, CompilerMacros).
  settings(
    assemblyOutputPath in assembly := file("Meta/Meta.jar"),
  )

lazy val ConfigEvaluator = (project in file("ConfigEvaluator")).
  settings(
    assemblyOutputPath in assembly := file("ConfigEvaluator/ConfigEvaluator.jar"),
  )
