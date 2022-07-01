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

object MakefileGenerator extends BuildfileGenerator {
  override def write() : Unit = {
    val printer = PrettyprintingManager.getPrinter("Makefile")

    val filesToConsider = PrettyprintingManager.getFiles ++ Settings.additionalFiles
    val cppFileNames = filesToConsider.filter(file => file.endsWith(".cpp")).toList.sorted
    val cuFileNames = filesToConsider.filter(file => file.endsWith(".cu")).toList.sorted
    val cppObjectFiles = cppFileNames.map(fn => fn.replace(".cpp", ".o"))
    val cuObjectFiles = cuFileNames.map(fn => fn.replace(".cu", ".o"))
    val allObjectFiles =
      if (Knowledge.cuda_enabled) cppObjectFiles ++ cuObjectFiles
      else cppObjectFiles

    printer <<< "CXX = " + Platform.resolveCompiler
    if (Knowledge.cuda_enabled)
      printer <<< "NVCC = " + Platform.resolveCudaCompiler
    printer <<< ""

    printer <<< mkStringTrimFlat(
      "CXXFLAGS =",
      Platform.resolveCFlags,
      Settings.makefile_additionalCFlags.mkString(" "),
      Settings.pathsInc.map(path => s"-I$path"),
      Settings.additionalDefines.map(path => s"-D$path"),
      "-I."
    )
    if (Knowledge.cuda_enabled)
      printer <<< mkStringTrimFlat(  // TODO: TPDL
        "CUDAFLAGS =",
        Platform.resolveCudaFlags,
        Settings.makefile_additionalCudaFlags.mkString(" "),
        Settings.pathsInc.map(path => s"-I$path"),
        "-I."
      )
    printer <<< mkStringTrimFlat(
      "LDFLAGS =",
      Platform.resolveLdFlags,
      Settings.makefile_additionalLDFlags.mkString(" "),
      Settings.pathsLib.map(path => s"-L$path"),
      Settings.additionalDefines.map(path => s"-D$path")
    )
    printer <<< mkStringTrimFlat(
      "LDLIBS =",
      CImg.resolveLdLibs(),
      IR_FileAccess_SIONlib.resolveLdLibs(),
      Settings.additionalLibs.map(lib => s"-l$lib")
    )
    printer <<< ""

    printer <<< mkStringTrimFlat("ALL_OBJ =", allObjectFiles)
    printer <<< ""

    printer <<< "BINARY = " + Settings.binary
    printer <<< ""

    if (Knowledge.cuda_enabled) {
      printer <<< "# define implicit rule for CUDA targets"
      printer <<< "%.o : %.cu"
      printer <<< "\t${NVCC} ${CUDAFLAGS} -c -o $@ $<"
      printer <<< ""
    }

    printer <<< ".PHONY: all"
    val allTarget = ListBuffer("all:", "${BINARY}")
    if (Settings.makefile_makeLibs)
      allTarget += "lib${BINARY}.a"
    printer <<< mkStringTrimFlat(allTarget)
    printer <<< ""

    printer <<< ".PHONY: clean"
    printer <<< "clean:"
    val cleanTargetCmd = ListBuffer("rm -f", "${BINARY}", "${ALL_OBJ}")
    if (Settings.makefile_makeLibs)
      cleanTargetCmd += "lib${BINARY}.a"
    printer <<< "\t" + mkStringTrimFlat(cleanTargetCmd)
    printer <<< ""

    printer <<< mkStringTrimFlat("${BINARY}:", "${ALL_OBJ}" + " " + Settings.makefile_additionalObjFiles.mkString(" "))
    printer <<< "\t" + mkStringTrimFlat("${CXX} -o ${BINARY}", "${LDFLAGS}", "${ALL_OBJ}", Settings.makefile_additionalObjFiles.mkString(" "), "${LDLIBS}")
    printer <<< ""

    if (Settings.makefile_makeLibs) {
      printer <<< mkStringTrimFlat("lib${BINARY}.a:", "${ALL_OBJ}")
      printer <<< "\t" + mkStringTrimFlat("ar -cvr ${BINARY}.a", "${ALL_OBJ}", "${LDLIBS}")
      printer <<< ""
    }
    printer <<< ""

    printer <<< "# begin: C++ targets"
    val cppPrettyPrinters = PrettyprintingManager.getPrettyprinters.filter(pp => pp.filename.endsWith(".cpp"))
      .toList.sortBy(f => f.filename)
    cppPrettyPrinters.foreach(pp => {
      printer <<< mkStringTrimFlat(
        pp.filename.replace(".cpp", ".o"), ":", pp.filename,
        PrettyprintingManager.Prettyprinter.gatherDependencies(pp)
      )
    })
    printer <<< "# end: C++ targets"
    printer <<< ""

    printer <<< "# begin: additionalFiles targets"
    // no information about dependencies => just compiling is the best we can do
    Settings.additionalFiles.filter(file => file.endsWith(".cpp")).toList.sorted.foreach(file => {
      printer <<< mkStringTrimFlat(file.replace(".cpp", ".o"), ":", file)
    })
    printer <<< "# end: additionalFiles targets"
    printer <<< ""

    if (Knowledge.cuda_enabled) {
      printer <<< "# begin: CUDA targets"
      val cudaPrettyPrinters = PrettyprintingManager.getPrettyprinters.filter(pp => pp.filename.endsWith(".cu"))
        .toList.sortBy(f => f.filename)
      cudaPrettyPrinters.foreach(pp => {
        printer <<< mkStringTrimFlat(
          pp.filename.replace(".cu", ".o"), ":", pp.filename,
          PrettyprintingManager.Prettyprinter.gatherDependencies(pp)
        )
      })
      printer <<< "# end: CUDA targets"
      printer <<< ""
    }

    printer.finish()
  }

  /** Concatenates a variable number of Strings or Seq[String] to a single String, separated by " ".
    *
    * String arguments are trimmed, Seq[String] arguments are mkString(" ")'ed.
    * White-space only strings are dropped.
    *
    * @param args variable number of String or Seq[String]
    */
  private def mkStringTrimFlat(args : Any*) : String = {
    args.map {
      case s : String        => s.trim
      case c : Iterable[Any] => c.mkString(" ")
      case x                 => throw new Exception("unexpected object " + x.getClass.getCanonicalName)

    }.filter(s => !s.isEmpty).mkString(" ")
  }
}