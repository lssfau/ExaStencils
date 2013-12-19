package exastencils.application

import scala.collection.mutable.ListBuffer
import java.io.PrintWriter
import java.io.File
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.primitives._

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

        s"MPI_Init(&argc, &argv);",

        // FIXME: move to specialized node
        s"int mpiRank;",
        s"int mpiSize;",
        s"MPI_Comm_rank(MPI_COMM_WORLD, &mpiRank);",
        s"MPI_Comm_size(MPI_COMM_WORLD, &mpiSize);",

        s"omp_set_num_threads(${Knowledge.numFragsPerBlock});",

        new ConditionStatement(s"argc != 1", ListBuffer[Statement](
          new ConditionStatement(s"0 == mpiRank" /*FIXME: use special condition*/ , ListBuffer[Statement](
            "LOG_NOTE(\"Usage:\");", // FIXME: use log nodes
            "LOG_NOTE(\"\\tmpirun -np (numBlocksTotal) .\\\\Poisson3D.exe\");")),
          s"MPI_Finalize();",
          s"return 1;")),

        s"StopWatch setupWatch;",

        s"initDomain(fragments);",

        new InitFields,
        """
	MPI_Barrier(MPI_COMM_WORLD);

	double lastRes = 0.0;
	{
		updateResidual""" + s"_${Knowledge.maxLevel}" + """(fragments, 0);
		double res = getGlobalResidual(fragments);

		res = res * res;
		double resTotal;

		MPI_Reduce(&res, &resTotal, 1, /*FIXME*/MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
		MPI_Bcast(&resTotal, 1, /*FIXME*/MPI_DOUBLE, 0, MPI_COMM_WORLD);

		resTotal = sqrt(resTotal);
		res = resTotal;

		lastRes = res;
	}
	double initialRes = lastRes;

	unsigned int curIt;
	unsigned int solSlots[""" + s"${Knowledge.numLevels}" + """];
	for (unsigned int s = 0; s <= """ + s"${Knowledge.maxLevel}" + """; ++s)
		solSlots[s] = 0;

	double minTime	= FLT_MAX;
	double maxTime	= 0;
	double meanTime	= 0;
	StopWatch stopWatch;
	StopWatch stopWatchTotal;

	for (curIt = 0; curIt < """ + s"${Knowledge.mgMaxNumIterations}" + """; ++curIt)
	{
		stopWatch.reset();

		performVCycle""" + s"_${Knowledge.maxLevel}" + """(fragments, solSlots);
	
		double tDuration = stopWatch.getTimeInMilliSecAndReset();
		minTime = std::min(minTime, tDuration);
		maxTime = std::max(maxTime, tDuration);
		meanTime += tDuration;

		// FIXME: res check every n cycles
 		if (0 != curIt % 1)
 			continue;

		double res = getGlobalResidual(fragments);
		res = res * res;
		double resTotal;

		MPI_Reduce(&res, &resTotal, 1, /*FIXME*/MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
		MPI_Bcast(&resTotal, 1, /*FIXME*/MPI_DOUBLE, 0, MPI_COMM_WORLD);
		
		resTotal = sqrt(resTotal);
		res = resTotal;

		if (0 == mpiRank)
		{
			LOG_NOTE("Iteration " << curIt << std::endl
				<< "\tCurrent residual (L2-norm), level " << """ + s"${Knowledge.maxLevel}" + """ << ": " << res << std::endl
				<< "\tRuntime for the current v-cycle: " << tDuration << std::endl
				<< "\tReduction: " << res / lastRes);
		}

		lastRes = res;

		if (res < 1e-8 * initialRes)
		{
			++curIt;
			break;
		}
	}

	for (int c = 0; c < mpiSize; ++c)
	{
		MPI_Barrier(MPI_COMM_WORLD);
		if (c == mpiRank)
		{
			double totalTime = stopWatchTotal.getTimeInMilliSecAndReset();
			/*LOG_NOTE("Parameters (coarsestLvl finestLvl numCoarseSteps numPreSmoothingSteps numPostSmoothingSteps omega:");
			LOG_NOTE(0 << " " << """ + s"${Knowledge.maxLevel}" + """ << " " << """ + s"${Knowledge.cgsNumSteps}" + """ << " " << """ + s"${Knowledge.smootherNumPre}" + """ << " " << """ + s"${Knowledge.smootherNumPost}" + """ << " " << """ + s"${Knowledge.smootherOmega}" + """);

			LOG_NOTE("Total Number of Cycles: " << curIt);
			LOG_NOTE("Total Execution Time:   " << totalTime.count());

			LOG_NOTE("Mean Time per Cycle:    " << (double)meanTime.count() / (double)curIt);
			LOG_NOTE("Min  Time per Cycle:    " << minTime.count());
			LOG_NOTE("Max  Time per Cycle:    " << maxTime.count());*/

			// if you need a more compact form of output:
			std::cout

			<< """" + s"${Knowledge.cgs}" + """" << "\t"
			<< """" + s"${Knowledge.smoother}" + """" << "\t"

				<< """ + s"${Knowledge.numBlocks_x}" + """ << "\t"
				<< """ + s"${Knowledge.numBlocks_y}" + """ << "\t"
				<< """ + s"${Knowledge.numBlocks_z}" + """ << "\t"
				<< """ + s"${Knowledge.numFragsPerBlock_x}" + """ << "\t"
				<< """ + s"${Knowledge.numFragsPerBlock_y}" + """ << "\t"
				<< """ + s"${Knowledge.numFragsPerBlock_z}" + """ << "\t"

				<< 0 << "\t"
				
				<< """ + s"${Knowledge.maxLevel}" + """ << "\t"
				<< """ + s"${Knowledge.cgsNumSteps}" + """ << "\t"
				<< """ + s"${Knowledge.smootherNumPre}" + """ << "\t"
				<< """ + s"${Knowledge.smootherNumPost}" + """ << "\t"
				<< """ + s"${Knowledge.smootherOmega}" + """ << "\t";//std::endl;
			std::cout
				<< curIt << "\t"
				<< totalTime << "\t"
				<< meanTime / (double)curIt << "\t"
				<< minTime << "\t"
				<< maxTime << std::endl;
		}

		if (1)		// if true, only the first process prints its measurements
			break;
	}
	
	MPI_Barrier(MPI_COMM_WORLD);
	if (0 == mpiRank)
		std::cout << std::endl;

	MPI_Barrier(MPI_COMM_WORLD);

	// FIXME: free primitives
	MPI_Finalize();

	return 0;

"""));
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
