package exastencils.prettyprinting

import exastencils.core.Settings
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

    printer <<< "include_directories(.)"
    printer <<< ""
    printer <<< "SET(SOURCES"
    cppFileNames.foreach(file => {
      printer <<< s"\t${file}"
    })
    printer <<< ")"
    printer <<< ""
    printer << "add_executable(" + Settings.binary + " ${SOURCES})"

    printer.finish
  }
}
