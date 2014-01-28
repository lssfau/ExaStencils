package exastencils.prettyprinting

import exastencils.knowledge._

object MakefileGenerator extends BuildfileGenerator {
  override def write : Unit = {
    val printer = PrettyprintingManager.getPrinter("Makefile")

    // TODO: switch by target hardware
    if (Knowledge.useOMP) {
      printer <<< "CXX = mpixlcxx_r"
    } else {
      printer <<< "CXX = mpixlcxx"
    }

    printer <<< "CFLAGS = -O3 -qhot -qarch=qp -qtune=qp -DNDEBUG" + (if (Knowledge.useOMP) " -qsmp=omp" else "")
    printer <<< "LFLAGS = -O3 -qhot -qarch=qp -qtune=qp -DNDEBUG" + (if (Knowledge.useOMP) " -qsmp=omp" else "") // TODO: check which flags are required
    printer <<< "BINARY = exastencils"
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