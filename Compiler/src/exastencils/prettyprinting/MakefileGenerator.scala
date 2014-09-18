package exastencils.prettyprinting

import exastencils.core._
import exastencils.knowledge._

object MakefileGenerator extends BuildfileGenerator {
  override def write : Unit = {
    val printer = PrettyprintingManager.getPrinter("Makefile")

    val filesToConsider = PrettyprintingManager.getFiles ++ Settings.additionalFiles
    val cppFileNames = filesToConsider.filter(file => file.endsWith(".cpp")).toList.sorted

    printer <<< "CXX = " + Hardware.compiler
    printer <<< "CFLAGS = " + Hardware.cflags + " " + Hardware.addcflags
    printer <<< "LFLAGS = " + Hardware.ldflags + " " + Hardware.addldflags

    printer <<< "BINARY = " + Settings.binary
    printer <<< ""

    printer <<< ".PHONY: all"
    printer <<< "all: exastencils"
    printer <<< ""

    printer <<< ".PHONY: clean"
    printer <<< "clean:"
    printer << "\trm -f "
    cppFileNames.foreach(file => { printer << s"${file.replace(".cpp", ".o")} " })
    printer <<< "${BINARY}"
    printer <<< ""

    printer << "exastencils: "
    cppFileNames.foreach(file => { printer << s"${file.replace(".cpp", ".o")} " })
    printer <<< ""
    printer << "\t${CXX} ${LFLAGS} -o ${BINARY} -I. "
    cppFileNames.foreach(file => { printer << s"${file.replace(".cpp", ".o")} " })
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

    printer <<< ""
    printer <<< ""

    printer.finish
  }
}