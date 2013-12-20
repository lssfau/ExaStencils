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

case class MultiGrid() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  var functions_HACK : ListBuffer[AbstractFunctionStatement] = new ListBuffer;

  override def printToFile = {
    val writerHeader = PrettyPrintManager.getPrinter(s"MultiGrid/MultiGrid.h");

    writerHeader << ("#ifndef MULTIGRID_MULTIGRID_H\n"
      + "#define MULTIGRID_MULTIGRID_H\n"
      + "#pragma warning(disable : 4800)\n"
      + "#include <mpi.h>\n"
      + "#include <vector>\n"
      + "#include \"Util/Log.h\"\n"
      + "#include \"Util/Vector.h\"\n"
      + "#include \"Util/Stopwatch.h\"\n"
      + "#include \"Primitives/Fragment3DCube.h\"\n"
      + "#include \"Primitives/CommunicationFunctions.h\"\n");

    for (func <- functions_HACK) {
      val function = func.asInstanceOf[FunctionStatement];
      writerHeader << s"${function.returntype.cpp} ${function.name}(" + function.parameters.map(param => s"${param.datatype.cpp} ${param.name}").mkString(", ") + ");\n";
    }

    writerHeader << "#endif // MULTIGRID_MULTIGRID_H\n";

    writerHeader.close(); // FIXME: finalize

    val writerSource = PrettyPrintManager.getPrinter(s"MultiGrid/MultiGrid.cpp");

    writerSource << "#include \"MultiGrid/MultiGrid.h\"\n";

    for (function <- functions_HACK)
      writerSource << function.cpp + "\n";

    writerSource.close(); // FIXME: finalize
  }
}
