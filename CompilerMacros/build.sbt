name := "CompilerMacros"

version := "1.0"

scalaVersion := "2.12.4"

scalaSource in Compile := baseDirectory.value / "src"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
