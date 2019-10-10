name := "Compiler"

version := "1.0"

sbtVersion := "1.3.2"

scalaVersion := "2.12.10"

scalaSource in Compile := baseDirectory.value / "src"

resourceDirectory in Compile := baseDirectory.value / "resources"

unmanagedBase in Compile := baseDirectory.value / "lib"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
