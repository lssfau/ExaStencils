package exastencils.prettyprinting

object MakefileGenerator extends BuildfileGenerator {
  override def write : Unit = {
    val printer = PrettyPrintManager.getPrinter("Makefile")

    printer <<< "CXX = g++"
    printer <<< "CFLAGS = -O3"
    printer <<< "BINARY = exastencils"
    printer <<< ""
    printer <<< "exastencils: "

    PrettyPrintManager.getFiles.filter(file => file != "Makefile").foreach(file => {
      printer << s"$file "
    })

    printer <<< "\t${CXX} ${CFLAGS} -o ${BINARY} "
    PrettyPrintManager.getFiles.filter(file => file != "Makefile").foreach(file => {
      printer << s"$file "
    })
    printer <<< ""
  }
}