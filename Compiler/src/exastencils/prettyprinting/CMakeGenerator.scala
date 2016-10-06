package exastencils.prettyprinting

import exastencils.config.Settings
import exastencils.logger.Logger

/** Rudimentary generator for cmake / CMakeLists.txt.
  *
  * The generated CMakeLists.txt is not sufficient for actually building an exastencils binary.
  * The intention is to support IDEs (e.g. qt-creator) for analyzing the generated code.
  *
  */
object CMakeGenerator extends BuildfileGenerator {
  override def write : Unit = {
    Logger.info("Generating CMakeLists.txt")
    val printer = PrettyprintingManager.getPrinter("CMakeLists.txt")
    val filesToConsider = PrettyprintingManager.getFiles ++ Settings.additionalFiles
    val cppFileNames = filesToConsider.filter(file => file.endsWith(".cpp")).toList.sorted
    printer <<< "cmake_minimum_required(VERSION 2.8)"
    printer <<< "message(WARNING " +
      "\"CMake support is not intended for building ExaStencils. " +
      "Use MakefileGenerator (make) or ProjectfileGenerator (Visual Studio) instead.\")"
    printer <<< "include_directories(.)"
    printer <<< ""
    printer <<< "SET(SOURCES"
    cppFileNames.foreach(file => {
      printer <<< s"\t${ file }"
    })
    printer <<< ")"
    printer <<< ""
    printer << "add_executable(" + Settings.binary + " ${SOURCES})"

    printer.finish
  }
}
