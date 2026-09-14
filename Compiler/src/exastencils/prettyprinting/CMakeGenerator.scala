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

import scala.collection.mutable.ListBuffer

import exastencils.config._
import exastencils.io.ir.IR_FileAccess_SIONlib
import exastencils.util.CImg

object CMakeGenerator extends BuildfileGenerator {

  override def write(): Unit = {
    val printer = PrettyprintingManager.getPrinter("CMakeLists.txt")

    val filesToConsider = PrettyprintingManager.getFiles ++ Settings.additionalFiles
    val headers = filesToConsider.filter(_.endsWith(".h")).toList.sorted
    val cppFiles = filesToConsider.filter(_.endsWith(".cpp")).toList.sorted
    val gpuFiles = filesToConsider.filter(_.endsWith(s".${ Platform.gpuFileExtension }")).toList.sorted

    val allSources = if (Knowledge.gpu_enabled) cppFiles ++ gpuFiles else cppFiles

    val target = Settings.binary

    /* project (and languages) */

    printer <<< "cmake_minimum_required(VERSION 3.26)"
    if (Settings.cmake_buildStandalone)
      printer <<< s"project($target LANGUAGES ${Settings.cmake_languages.mkString(" ")})"
    printer <<< ""

    val gpuFlags = if (Knowledge.gpu_enabled) Platform.resolveGpuFlags else ""

    // language and architecture of the generated device code files
    val gpuLang = Platform.gpuBackend
    val gpuArch = Platform.gpuArchitecture

    /* find packages */

    Settings.cmake_additionalPackages foreach { pkg =>
      printer <<< s"find_package($pkg REQUIRED)"
    }
    printer <<< ""

    /* sources */

    printer <<< "set(SOURCES"
    allSources.foreach(f => printer <<< s"  $f")
    printer <<< ")"
    printer <<< ""

    /* headers */

    printer <<< "set(HEADERS"
    headers.foreach(f => printer <<< s"  $f")
    printer <<< ")"
    printer <<< ""

    /* target */

    if (Settings.cmake_buildStandalone) {
      // build executable from sources
      printer <<< s"add_executable($target)"
    } else {
      // build library that can be used by other projects
      printer <<< s"add_library($target)"
    }
    printer <<< s"target_sources($target PRIVATE $${SOURCES})"
    printer <<< s"target_sources($target PUBLIC $${HEADERS})"
    printer <<< ""

    /* includes */

    printer <<< s"target_include_directories($target PRIVATE"
    (Settings.pathsInc :+ ".").foreach(d => printer <<< s"  $d")
    printer <<< ")"
    printer <<< ""

    /* defines */

    if (Settings.additionalDefines.nonEmpty) {
      printer <<< s"target_compile_definitions($target PRIVATE"
      Settings.additionalDefines.foreach(d => printer <<< s"  $d")
      printer <<< ")"
      printer <<< ""
    }

    /* set CXX standard */

    printer <<< s"target_compile_features($target PRIVATE cxx_std_${Settings.cxx_standard})"

    /* set GPU architecture */

    if (Knowledge.gpu_enabled)
      printer <<< s"set_target_properties($target PROPERTIES ${ gpuLang }_ARCHITECTURES $gpuArch)\n"

    /* set compile flags */

    // omit duplicate handling for CXX standard and OpenMP flags
    val filteredCFlags = filterFlags(Platform.resolveCFlags, remove = Seq("openmp", "std=c++"))
    if (filteredCFlags.nonEmpty) {
      printer <<< s"target_compile_options($target PRIVATE"
      printer <<< s"""  $$<$$<COMPILE_LANGUAGE:CXX>:${filteredCFlags}>"""
      printer <<< ")"
      printer <<< ""
    }

    if (Knowledge.gpu_enabled) {
      // omit duplicate handling for CXX standard and device architecture
      val filteredGpuFlags = filterFlags(gpuFlags, remove = Seq("arch", "std=c++"))
      if (filteredGpuFlags.nonEmpty) {
        printer <<< s"target_compile_options($target PRIVATE"
        printer <<< s"""  $$<$$<COMPILE_LANGUAGE:$gpuLang>:${filteredGpuFlags}>"""
        printer <<< ")"
        printer <<< ""
      }
    }

    /* link directories `*/

    if (Settings.pathsLib.nonEmpty) {
      printer <<< s"target_link_directories($target PRIVATE"
      Settings.pathsLib.foreach(d => printer <<< s"  $d")
      printer <<< ")"
      printer <<< ""
    }

    /* link libraries */

    var libs: ListBuffer[String] = mkStringTrimFlat(
      CImg.resolveLdLibs(),
      IR_FileAccess_SIONlib.resolveLdLibs(),
      Settings.additionalLibs.map(lib => s"-l$lib")
    ).split(" ").to[ListBuffer]

    // replace parallelization libs with modern CMake targets
    libs -= "-lcuda"
    libs -= "-lcudart"
    libs -= "-lamdhip64"
    libs -= "-lmpi"
    if (Knowledge.gpu_enabled)
      libs += (if (Knowledge.hip_enabled) "hip::device" else "CUDA::cudart")
    if (Knowledge.mpi_enabled)
      libs += "MPI::MPI_CXX"
    if (Knowledge.omp_enabled)
      libs += "OpenMP::OpenMP_CXX"

    if (libs.nonEmpty) {
      printer <<< s"target_link_libraries($target PRIVATE"
      libs.distinct.foreach(l => printer <<< s"  $l")
      printer <<< ")"
      printer <<< ""
    }

    printer.finish()
  }

  /* helper funcs */

  private def filterFlags(flags: String, remove: Seq[String]): String = {
    flags.split(" ")
      .filterNot(f => remove.exists(r => f.contains(r)))
      .mkString(" ")
  }

  private def mkStringTrimFlat(args: Any*): String = {
    args.map {
      case s: String        => s.trim
      case c: Iterable[Any] => c.mkString(" ")
      case x                => throw new Exception("unexpected object " + x.getClass.getCanonicalName)
    }.filter(_.nonEmpty).mkString(" ")
  }
}