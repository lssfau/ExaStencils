

name := "Compiler"

version := "1.0"

sbtVersion := "1.3.2"

scalaVersion := "2.12.10"

scalaSource in Compile := baseDirectory.value / "src"

resourceDirectory in Compile := baseDirectory.value / "res"

unmanagedBase in Compile := baseDirectory.value / "lib"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "io.github.kostaskougios" % "cloning" % "1.10.0"
libraryDependencies += "org.objenesis" % "objenesis" % "3.1"

lazy val downloadCImg = taskKey[Unit]("Download CImg header and store it in Compiler/res")

downloadCImg := {
  val sourceUrl = "https://raw.githubusercontent.com/dtschump/CImg/master/CImg.h"
  val targetFileName = "Compiler/res/CImg.h"
  val overwrite = true

  if (overwrite || java.nio.file.Files.notExists(new File(targetFileName).toPath)) {
    println(s"Downloading CImg header from $sourceUrl")

    val targetFile = new java.io.File(targetFileName)
    if (!targetFile.getParentFile.exists()) {
      println("Setting up directory")
      targetFile.getParentFile.mkdirs()
    }

    try {
      val src = scala.io.Source.fromURL(sourceUrl)
      val out = new java.io.FileWriter(targetFile)
      out.write(src.mkString)
      out.close()
    } catch {
      case e : java.io.IOException => println(s"Error downloading: $e")
    }

    println("Successful")
  } else {
    println("CImg header already exists")
  }
}
