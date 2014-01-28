package exastencils.prettyprinting

import exastencils.core.Settings
import exastencils.knowledge._

object MakefileGenerator extends BuildfileGenerator {
  override def write : Unit = {
    val printer = PrettyprintingManager.getPrinter("Makefile")

    // TODO: switch by target hardware
    if (Knowledge.useOMP) {
      printer <<< "CXX = mpixlcxx_r"
      printer <<< "CFLAGS = " + Hardware.cflags_openmp
      printer <<< "LFLAGS = " + Hardware.ldflags_openmp
    } else {
      printer <<< "CXX = mpixlcxx"
      printer <<< "CFLAGS = " + Hardware.cflags_mpi
      printer <<< "LFLAGS = " + Hardware.ldflags_mpi
    }

    printer <<< "BINARY = " + Settings.binary
    printer <<< ""

    printer <<< ".PHONY: all"
    printer <<< "all: exastencils"
    printer <<< ""

    printer <<< ".PHONY: clean"
    printer <<< "clean:"
    printer << "\trm -f "
    PrettyprintingManager.getFiles.filter(file => file.endsWith(".cpp")).foreach(file => { printer << s"${file.replace(".cpp", ".o")} " })
    printer <<< "${BINARY}"
    printer <<< ""

    printer << "exastencils: "
    PrettyprintingManager.getFiles.filter(file => file.endsWith(".cpp")).foreach(file => { printer << s"${file.replace(".cpp", ".o")} " })
    printer <<< ""
    printer << "\t${CXX} ${LFLAGS} -o ${BINARY} -I. "
    PrettyprintingManager.getFiles.filter(file => file.endsWith(".cpp")).foreach(file => { printer << s"${file.replace(".cpp", ".o")} " })
    printer <<< ""
    printer <<< ""

    PrettyprintingManager.getPrettyprinters.filter(pp => pp.filename.endsWith(".cpp")).foreach(pp => {
      printer << s"${pp.filename.replace(".cpp", ".o")}: ${pp.filename} "
      pp.dependencies.foreach(dep => printer << s"${dep.filename} ")
      printer <<< " "
      printer <<< "\t${CXX} ${CFLAGS} -c -o " + pp.filename.replace(".cpp", ".o") + " -I. " + pp.filename
    })
    printer <<< ""
    printer <<< ""

    printer.close
  }
}