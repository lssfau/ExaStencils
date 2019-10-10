name := "ConfigEvaluator"

version := "1.0"

sbtVersion := "1.3.2"

scalaVersion := "2.12.10"

scalaSource in Compile := baseDirectory.value / "src"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
