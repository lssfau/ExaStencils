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
    val fieldCollection = StateManager.findFirst[FieldCollection]().get
    val fields = fieldCollection.fields
    var statements : ListBuffer[Statement] = new ListBuffer

    for (field <- fields) {
      statements += new LoopOverFragments(field.domain,
        new LoopOverDimensions(new IndexRange(
          new MultiIndex(field.layout(0).idxGhostLeftBegin, field.layout(1).idxGhostLeftBegin, field.layout(2).idxGhostLeftBegin),
          new MultiIndex(field.layout(0).idxGhostRightEnd, field.layout(1).idxGhostRightEnd, field.layout(2).idxGhostRightEnd)),
          (0 until field.numSlots).to[ListBuffer].map(slot =>
            new AssignmentStatement(
              new DirectFieldAccess("curFragment.", field, slot, DefaultLoopMultiIndex()),
              0.0) : Statement)) with OMP_PotentiallyParallel with PolyhedronAccessable) with OMP_PotentiallyParallel
    }

    // FIXME: get this info from one of the DSLs
    // special treatment for the finest solution
    {
      val field = fieldCollection.getFieldByIdentifier("Solution", Knowledge.maxLevel).get
      statements += new LoopOverFragments(field.domain, ListBuffer[Statement](
        s"std::srand((unsigned int)fragments[f]->id)",
        new LoopOverDimensions(new IndexRange(
          new MultiIndex(field.layout(0).idxDupLeftBegin, field.layout(1).idxDupLeftBegin, field.layout(2).idxDupLeftBegin),
          new MultiIndex(field.layout(0).idxDupRightEnd, field.layout(1).idxDupRightEnd, field.layout(2).idxDupRightEnd)),
          ListBuffer[Statement](
            new AssignmentStatement("double val", "(double)std::rand()" / "RAND_MAX")) ++
            (0 until field.numSlots).to[ListBuffer].map(slot =>
              new AssignmentStatement(
                new DirectFieldAccess("curFragment.", field, slot, DefaultLoopMultiIndex()),
                s"val") : Statement))))
    }

    StatementBlock(statements)
  }
}

case class Poisson3DMain() extends AbstractFunctionStatement with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = Poisson3DMain\n"

  override def expand : FunctionStatement = {
    // FIXME: make the next line of code more readable and robust
    val globals : Globals = StateManager.root.asInstanceOf[Root].nodes.find(node => node.isInstanceOf[Globals]).get.asInstanceOf[Globals]

    globals.variables += new VariableDeclarationStatement(new VariableAccess(s"fragments[${Knowledge.domain_numFragsPerBlock}]", Some("Fragment3DCube*")))

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

          new InitFields,

          new MPI_Barrier,

          "Application()", // TODO: think about inlining the App

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
