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
import exastencils.omp._
import exastencils.polyhedron._

case class InitFieldsWithZero() extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = InitFieldsWithZero\n"
  override def cpp_decl : String = cpp

  def expand() : FunctionStatement = {
    val fields = FieldCollection.fields
    var statements : ListBuffer[Statement] = new ListBuffer

    for (field <- fields) {
      statements += new LoopOverFragments(field.domain.index,
        new LoopOverDimensions(Knowledge.dimensionality + 1, new IndexRange(
          new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => field.layout(i).idxGhostLeftBegin)),
          new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => field.layout(i).idxGhostRightEnd))),
          (0 until field.numSlots).to[ListBuffer].map(slot =>
            new AssignmentStatement(
              new DirectFieldAccess(FieldSelection(field, slot, -1), LoopOverDimensions.defIt),
              0.0) : Statement)) with OMP_PotentiallyParallel with PolyhedronAccessable) with OMP_PotentiallyParallel
    }

    new FunctionStatement(new UnitDatatype, s"initFieldsWithZero", ListBuffer[VariableAccess](), statements)
  }
}

case class MultiGrid(var functions : ListBuffer[AbstractFunctionStatement] = new ListBuffer) extends Node with FilePrettyPrintable {
  if (Knowledge.data_initAllFieldsWithZero)
    functions += new InitFieldsWithZero

  override def printToFile = {
    val writer = PrettyprintingManager.getPrinter(s"MultiGrid/MultiGrid.h")

    writer << (
      (if (Knowledge.useMPI) "#pragma warning(disable : 4800)\n" else "")
      + (if (Knowledge.useMPI) "#include <mpi.h>\n" else "")
      + "#include \"Globals/Globals.h\"\n"
      + "#include \"Util/Log.h\"\n"
      + "#include \"Util/Vector.h\"\n"
      + "#include \"Util/Stopwatch.h\"\n"
      + "#include \"Primitives/CommunicationFunctions.h\"\n"
      + "#include \"Domains/DomainGenerated.h\"\n")

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
