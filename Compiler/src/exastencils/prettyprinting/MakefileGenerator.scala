package exastencils.prettyprinting

object MakefileGenerator extends BuildfileGenerator {
  override def write : Unit = {
    val printer = PrettyprintingManager.getPrinter("Makefile")

    // TODO: switch by target hardware
    printer <<< "CXX = mpixlcxx_r"
    printer <<< "CFLAGS = -O3 -qhot -qarch=qp -qtune=qp -qsmp=omp -DNDEBUG"
    printer <<< "LFLAGS = -O3 -qhot -qarch=qp -qtune=qp -qsmp=omp -DNDEBUG"	// TODO: check which flags are required
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

    // FIXME: header dependencies are not taken into account 
    PrettyprintingManager.getFiles.filter(file => file.endsWith(".cpp")).foreach(file => {
      printer <<< s"${file.replace(".cpp", ".o")}: $file"
      printer <<< "\t${CXX} ${CFLAGS} -c -o " + file.replace(".cpp", ".o") + " -I. " + file
    })
    printer <<< ""
    printer <<< ""

    printer.close
  }
}