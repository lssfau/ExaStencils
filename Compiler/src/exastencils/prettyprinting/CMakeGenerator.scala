//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

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
  override def write() : Unit = {
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

    printer.finish()
  }
}
