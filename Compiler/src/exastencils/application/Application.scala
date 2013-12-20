package exastencils.application

import scala.collection.mutable.ListBuffer
import java.io.PrintWriter
import java.io.File
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.primitives._
import exastencils.mpi._

case class InitFields() extends Statement {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = InitFields\n";

  def expandSpecial(fields : FieldCollection) : LoopOverFragments = {
    var body : ListBuffer[Statement] = new ListBuffer;

    // FIXME: setting the seed here will only yield reproducible results if OMP is deactivated
    body += s"std::srand((unsigned int)fragments[f]->id);";

    for (level <- 0 to Knowledge.maxLevel) {
      for (field <- fields.fields) {
        body += new LoopOverDimensions(fieldToIndexInnerWide(Array(0, 0, 0), level),
          (0 until field.numSlots).to[ListBuffer].map(slot =>
            new AssignmentStatement(
              new FieldAccess(field, NumericLiteral(level), NumericLiteral(slot), Mapping.access(level)),
              NumericLiteral(0.0)) : Statement));
      }
    }

    // special treatment for the finest level 
    for (field <- fields.fields) {
      // FIXME: init by given parameters
      if ("Solution" == field.name)
        body += new LoopOverDimensions(fieldToIndexInner(Array(0, 0, 0), Knowledge.maxLevel), ListBuffer[Statement](
          s"double val = (double)std::rand() / RAND_MAX;")
          ++
          (0 until field.numSlots).to[ListBuffer].map(slot =>
            new AssignmentStatement(
              new FieldAccess(field, NumericLiteral(Knowledge.maxLevel), NumericLiteral(slot), Mapping.access(Knowledge.maxLevel)),
              s"val") : Statement));
    }

    return new LoopOverFragments(body);
  }
}

case class Poisson3DMain() extends AbstractFunctionStatement with Expandable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = "NOT VALID ; CLASS = Poisson3DMain\n";

  override def expand : FunctionStatement = {
    new FunctionStatement("int", "main", ListBuffer(Variable("int", "argc"), Variable("char**", "argv")),
      ListBuffer[Statement](
        s"std::vector<Fragment3DCube*> fragments;", //FIXME: move to global space // FIXME: convert to fixed size array

        new MPI_Init,
        new MPI_SetRankAndSize,

        s"omp_set_num_threads(${Knowledge.numFragsPerBlock});",

        new ConditionStatement(s"argc != 1", ListBuffer[Statement](
          new ConditionStatement(new MPI_IsRootProc,
            ListBuffer[Statement](
              "LOG_NOTE(\"Usage:\");", // FIXME: use log nodes
              "LOG_NOTE(\"\\tmpirun -np (numBlocksTotal) .\\\\Poisson3D.exe\");")),
          s"MPI_Finalize();",
          s"return 1;")),

        s"StopWatch setupWatch;",

        s"initDomain(fragments);",

        new InitFields,

        new MPI_Barrier,

        s"updateResidual_${Knowledge.maxLevel}(fragments, 0);",
        s"double lastRes = getGlobalResidual(fragments);",
        s"double initialRes = lastRes;",

        s"unsigned int curIt;",
        s"unsigned int solSlots[${Knowledge.numLevels}];",
        new ForLoopStatement(s"unsigned int s = 0", s"s <= ${Knowledge.maxLevel}", "++s",
          s"solSlots[s] = 0;"),

        s"double minTime = FLT_MAX;",
        s"double maxTime = 0;",
        s"double meanTime = 0;",
        s"StopWatch stopWatch;",
        s"StopWatch stopWatchTotal;",

        new ForLoopStatement(s"curIt = 0", s"curIt < ${Knowledge.mgMaxNumIterations}", s"++curIt",
          ListBuffer[Statement](
            s"stopWatch.reset();",

            s"performVCycle_${Knowledge.maxLevel}(fragments, solSlots);",

            s"double tDuration = stopWatch.getTimeInMilliSecAndReset();",
            s"minTime = std::min(minTime, tDuration);",
            s"maxTime = std::max(maxTime, tDuration);",
            s"meanTime += tDuration;",

            s"double res = getGlobalResidual(fragments);",

            new ConditionStatement(new MPI_IsRootProc,
              "LOG_NOTE(\"Iteration \" << curIt << \"\\n\\tCurrent residual (L2-norm): \" << res << \"\\n\\tRuntime for the current v-cycle: \" << tDuration << \"\\n\\tReduction: \" << res / lastRes);"),

            s"lastRes = res;",

            // FIXME: set this criterion externally
            new ConditionStatement("res < 1e-8 * initialRes", ListBuffer[Statement](
              s"++curIt;",
              s"break;")))),

        new MPI_Barrier,

        new ConditionStatement(new MPI_IsRootProc,
          // FIXME: extract to separate node / think about how to determine which info to print automatically
          // FIXME: think about a more intuitive way of printing information
          ListBuffer[Statement](
            s"double totalTime = stopWatchTotal.getTimeInMilliSecAndReset();",
            "std::cout << "
              + "\"" + Knowledge.cgs + "\\t\" << "
              + "\"" + Knowledge.smoother + "\\t\" << "
              + Knowledge.numBlocks_x + " << \"\\t\" << "
              + Knowledge.numBlocks_y + " << \"\\t\" << "
              + Knowledge.numBlocks_z + " << \"\\t\" << "
              + Knowledge.numFragsPerBlock_x + " << \"\\t\" << "
              + Knowledge.numFragsPerBlock_y + " << \"\\t\" << "
              + Knowledge.numFragsPerBlock_z + " << \"\\t\" << "
              + 0 + " << \"\\t\" << "
              + Knowledge.maxLevel + " << \"\\t\" << "
              + Knowledge.cgsNumSteps + " << \"\\t\" << "
              + Knowledge.smootherNumPre + " << \"\\t\" << "
              + Knowledge.smootherNumPost + " << \"\\t\" << "
              + Knowledge.smootherOmega + " << \"\\t\" << "
              + "curIt << \"\\t\" << "
              + "totalTime << \"\\t\" << "
              + "meanTime << \"\\t\" << "
              + "minTime << \"\\t\" << "
              + "maxTime << \"\\n\";")),

        new MPI_Barrier,

        new ConditionStatement(new MPI_IsRootProc,
          s"std::cout << std::endl;"),

        // FIXME: free primitives
        new MPI_Finalize,

        s"return 0;"));
  }
}

case class Poisson3D() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  var functions_HACK : ListBuffer[AbstractFunctionStatement] = new ListBuffer;
  functions_HACK += new Poisson3DMain;

  override def printToFile = {
    val writerSource = new PrintWriter(new File(Globals.printPath + s"Poisson3D.cpp"));

    writerSource.write(
      "#pragma warning(disable : 4800)\n"
        + "#include <mpi.h>\n"
        + "#include <vector>\n"
        + "#include <iostream>\n"
        + "#include <omp.h>\n"
        + "#include \"Util/Log.h\"\n"
        + "#include \"Util/Vector.h\"\n"
        + "#include \"Util/Stopwatch.h\"\n"
        + "#include \"MultiGrid/MultiGrid.h\"\n"
        + "#include \"Domains/DomainGenerated.h\"\n");

    for (function <- functions_HACK)
      writerSource.write(function.cpp + "\n");

    writerSource.close();

  }
}
