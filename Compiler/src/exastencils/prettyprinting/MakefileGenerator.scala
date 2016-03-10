package exastencils.prettyprinting

import exastencils.core._
import exastencils.knowledge._

object MakefileGenerator extends BuildfileGenerator {
  override def write : Unit = {
    val printer = PrettyprintingManager.getPrinter("Makefile")

    val filesToConsider = PrettyprintingManager.getFiles ++ Settings.additionalFiles
    val cppFileNames = filesToConsider.filter(file => file.endsWith(".cpp")).toList.sorted
    val cuFileNames = filesToConsider.filter(file => file.endsWith(".cu")).toList.sorted

    printer <<< "CXX = " + Platform.resolveCompiler
    if (Knowledge.experimental_cuda_enabled)
      printer <<< "NVCC = nvcc" // TODO: TPDL
    printer <<< ""

    printer <<< "CFLAGS = " + Platform.resolveCFlags + " " +
      Settings.pathsInc.map(path => s"-I$path").mkString(" ") + " " +
      Settings.additionalDefines.map(path => s"-D$path").mkString(" ")
    if (Knowledge.experimental_cuda_enabled)
      printer <<< "NVCCFLAGS = -std=c++11 -O3 -DNDEBUG " + Settings.pathsInc.map(path => s"-I$path").mkString(" ") + " " // TODO: TPDL
    printer <<< "LFLAGS = " + Platform.resolveLdFlags + " " +
      Settings.pathsLib.map(path => s"-L$path").mkString(" ") + " " +
      Settings.additionalDefines.map(path => s"-D$path").mkString(" ")
    printer <<< ""

    printer <<< "BINARY = " + Settings.binary
    printer <<< ""

    printer <<< ".PHONY: all"
    printer <<< "all: ${BINARY}"
    printer <<< ""

    printer <<< ".PHONY: clean"
    printer <<< "clean:"
    printer << "\trm -f "
    cppFileNames.foreach(file => { printer << s"${file.replace(".cpp", ".o")} " })
    printer <<< "${BINARY}"
    printer <<< ""

    printer << "${BINARY}: "
    cppFileNames.foreach(file => { printer << s"${file.replace(".cpp", ".o")} " })
    if (Knowledge.experimental_cuda_enabled)
      cuFileNames.foreach(file => { printer << s"${file.replace(".cu", ".o")} " })
    printer <<< ""
    printer << "\t${CXX}"
    printer << " -o ${BINARY} -I. "
    cppFileNames.foreach(file => { printer << s"${file.replace(".cpp", ".o")} " })
    if (Knowledge.experimental_cuda_enabled)
      cuFileNames.foreach(file => { printer << s"${file.replace(".cu", ".o")} " })
    Settings.additionalLibs.foreach(lib => { printer << s"-l$lib " })
    printer <<< " ${LFLAGS}"
    printer <<< ""
    printer <<< ""

    PrettyprintingManager.getPrettyprinters.filter(pp => pp.filename.endsWith(".cpp")).toList.sortBy(f => f.filename).foreach(pp => {
      printer << s"${pp.filename.replace(".cpp", ".o")}: ${pp.filename} "
      PrettyprintingManager.Prettyprinter.gatherDependencies(pp).foreach(dep => printer << s"$dep ")
      printer <<< " "
      printer <<< "\t${CXX} ${CFLAGS} -c -o " + pp.filename.replace(".cpp", ".o") + " -I. " + pp.filename
    })

    // no information about dependecies => just compiling is the best we can do
    Settings.additionalFiles.filter(file => file.endsWith(".cpp")).toList.sorted.foreach(file => {
      printer <<< s"${file.replace(".cpp", ".o")}: ${file} "
      printer <<< "\t${CXX} ${CFLAGS} -c -o " + file.replace(".cpp", ".o") + " -I. " + file
    })

    if (Knowledge.experimental_cuda_enabled) {
      PrettyprintingManager.getPrettyprinters.filter(pp => pp.filename.endsWith(".cu")).toList.sortBy(f => f.filename).foreach(pp => {
        printer << s"${pp.filename.replace(".cu", ".o")}: ${pp.filename} "
        PrettyprintingManager.Prettyprinter.gatherDependencies(pp).foreach(dep => printer << s"$dep ")
        printer <<< " "
        printer <<< "\t${NVCC} ${NVCCFLAGS} -c -o " + pp.filename.replace(".cu", ".o") + " -I. " + pp.filename
      })
    }

    printer <<< ""
    printer <<< ""

    printer.finish
  }
}
