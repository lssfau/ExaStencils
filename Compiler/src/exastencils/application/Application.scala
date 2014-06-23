package exastencils.application

import scala.collection.mutable.ListBuffer
import java.io.PrintWriter
import java.io.File
import exastencils.core._
import exastencils.core.collectors._
import exastencils.knowledge._
import exastencils.globals._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.primitives._
import exastencils.prettyprinting._
import exastencils.mpi._
import exastencils.omp._
import exastencils.polyhedron._

case class InitFields() extends Statement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = InitFields\n"

  def expand() : StatementBlock = {
    val fields = FieldCollection.fields
    var statements : ListBuffer[Statement] = new ListBuffer

    for (field <- fields) {
      statements += new LoopOverFragments(field.domain.index,
        new LoopOverDimensions(Knowledge.dimensionality + 1, new IndexRange(
          new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => field.layout(i).idxGhostLeftBegin)),
          new MultiIndex((0 until Knowledge.dimensionality + 1).toArray.map(i => field.layout(i).idxGhostRightEnd))),
          (0 until field.numSlots).to[ListBuffer].map(slot =>
            new AssignmentStatement(
              new DirectFieldAccess(FieldSelection(field, slot, -1), DefaultLoopMultiIndex()),
              0.0) : Statement)) with OMP_PotentiallyParallel with PolyhedronAccessable) with OMP_PotentiallyParallel
    }

    StatementBlock(statements)
  }
}

case class Poisson3DMain() extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = Poisson3DMain\n"
  override def cpp_decl = cpp

  override def expand : FunctionStatement = {
    // FIXME: make the next line of code more readable and robust
    val globals : Globals = StateManager.root.asInstanceOf[Root].nodes.find(node => node.isInstanceOf[Globals]).get.asInstanceOf[Globals]

    new FunctionStatement("int", "main", ListBuffer(VariableAccess("argc", Some("int")), VariableAccess("argv", Some("char**"))),
      (if (Knowledge.useMPI)
        ListBuffer[Statement]()
      else ListBuffer[Statement]()) ++
        ListBuffer[Statement](
          new MPI_Init,

          "initGlobals()",

          //if (Knowledge.useOMP) s"omp_set_num_threads(${Knowledge.omp_numThreads})" else "",

          new ConditionStatement(s"argc != 1", ListBuffer[Statement](
            new ConditionStatement(new MPI_IsRootProc,
              ListBuffer[Statement](
                "LOG_NOTE(\"Usage:\")", // FIXME: use log nodes
                "LOG_NOTE(\"\\tmpirun -np (numBlocksTotal) .\\\\Poisson3D.exe\")")),
            new MPI_Finalize,
            s"return 1")),

          s"StopWatch setupWatch",

          s"initDomain()",

          (if (Knowledge.data_initAllFieldsWithZero) new InitFields else new NullStatement),

          new MPI_Barrier,

          "Application()", // TODO: think about inlining the App

          "destroyGlobals()",
          
          new MPI_Finalize,

          s"return 0"))
  }
}

case class Poisson3D() extends Node with FilePrettyPrintable {
  var functions_HACK : ListBuffer[AbstractFunctionStatement] = new ListBuffer
  functions_HACK += new Poisson3DMain

  override def printToFile = {
    val writer = PrettyprintingManager.getPrinter(s"Poisson3D.cpp")

    writer << (
      (if (Knowledge.useMPI) "#pragma warning(disable : 4800)\n" else "")
      + (if (Knowledge.useMPI) "#include <mpi.h>\n" else "")
      + "#include <iostream>\n"
      + "#include <cstdlib>\n"
      + "#include <cfloat>\n"
      + (if (Knowledge.useOMP) "#include <omp.h>\n" else "")
      + "#include \"Globals/Globals.h\"\n"
      + "#include \"Util/Log.h\"\n"
      + "#include \"Util/Vector.h\"\n"
      + "#include \"Util/Stopwatch.h\"\n"
      + "#include \"MultiGrid/MultiGrid.h\"\n"
      + "#include \"Domains/DomainGenerated.h\"\n")

    for (function <- functions_HACK)
      writer << function.cpp + "\n"
  }
}
