package exastencils.multiGrid

import java.io.PrintWriter
import java.io.File
import scala.collection.mutable.ListBuffer
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.multiGrid._
import exastencils.prettyprinting._

case class MultiGrid(var functions : ListBuffer[AbstractFunctionStatement] = new ListBuffer) extends Node with FilePrettyPrintable {
  override def printToFile = {
    val writer = PrettyprintingManager.getPrinter(s"MultiGrid/MultiGrid.h")

    writer << (
      (if (Knowledge.useMPI) "#pragma warning(disable : 4800)\n" else "")
      + (if (Knowledge.useMPI) "#include <mpi.h>\n" else "")
      + "#include \"Globals/Globals.h\"\n"
      + "#include \"Util/Log.h\"\n"
      + "#include \"Util/Vector.h\"\n"
      + "#include \"Util/Stopwatch.h\"\n"
      + "#include \"Primitives/CommunicationFunctions.h\"\n")

    for (func <- functions) {
      val function = func.asInstanceOf[FunctionStatement]
      writer << s"${function.returntype.cpp} ${function.name.cpp}(" + function.parameters.map(param => s"${param.dType.get.cpp} ${param.name}").mkString(", ") + ");\n"
    }

    var i = 0
    for (f <- functions) {
      var s : String = ""

      val writer = PrettyprintingManager.getPrinter(s"MultiGrid/MultiGrid_$i.cpp")

      writer << "#include \"MultiGrid/MultiGrid.h\"\n"
      writer << f.cpp + "\n"

      i += 1
    }
  }
}
