name := "Compiler"

version := "1.0"

scalaVersion := "2.12.4"

scalaSource in Compile := baseDirectory.value / "src"

resourceDirectory in Compile := baseDirectory.value / "resources"

unmanagedBase in Compile := baseDirectory.value / "lib"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6"
