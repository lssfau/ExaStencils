package exastencils.prettyprinting

object MakefileGenerator extends BuildfileGenerator {
  override def write : Unit = {
    val printer = PrettyprintingManager.getPrinter("Makefile")

    printer <<< "CXX = mpic++"
    printer <<< "CFLAGS = -O3"
    printer <<< "BINARY = exastencils"
    printer <<< ""
    printer <<< "exastencils: "

    PrettyprintingManager.getFiles.filter(file => file != "Makefile").foreach(file => {
      printer << s"$file "
    })

    printer <<< "\t${CXX} ${CFLAGS} -o ${BINARY} "
    PrettyprintingManager.getFiles.filter(file => file.endsWith(".cpp")).foreach(file => {
      printer << s"$file "
    })
    printer <<< ""
    
    printer.close
  }
}