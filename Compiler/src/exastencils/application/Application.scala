package exastencils.application

import java.io.PrintWriter
import java.io.File

import exastencils.knowledge._

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

case class Poisson3D() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def printToFile = {
    val writerHeader = new PrintWriter(new File(Globals.printPath + s"Poisson3D.h"));

    writerHeader.write("""
#ifndef	POISSON_3D_H
#define	POISSON_3D_H

#include "Util/Defines.h"				// required to set USE_* defines

#pragma warning(disable : 4800)
#include <mpi.h>				// required to be included before stdio.h

#include <vector>
#include <iostream>

#include <omp.h>

#include "Util/Defines.h"
#include "Util/Log.h"
#include "Util/TypeDefs.h"

#include "Util/Vector.h"
#include "Util/Stopwatch.h"

#include "MultiGrid/MultiGrid.h"

#include "Domains/DomainGenerated.h"

#endif	// POISSON_3D_H
""");

    writerHeader.close();

    val writerSource = new PrintWriter(new File(Globals.printPath + s"Poisson3D.cpp"));

    writerSource.write("""
#include "Poisson3D.h"

#ifdef TIME_MEASUREMENTS
	unsigned int FINAL_LEVEL				= 8;
	unsigned int COARSE_LEVEL				= 0;
	unsigned int NUM_COARSE_STEPS			= 64;
	unsigned int NUM_PRE_SMOOTHING_STEPS	= 2;
	unsigned int NUM_POST_SMOOTHING_STEPS	= 2;
	double		 OMEGA						= 0.8;
	unsigned int NUM_LSE_ENTRIES_PER_RANK	= 64;
	unsigned int COARSE_GRID_RANK_STRIDE	= 4;
#endif

void printUsage ()
{
#ifdef USE_MPI
	LOG_NOTE("Usage:");
#	ifdef TIME_MEASUREMENTS
	LOG_NOTE("\tmpirun -np (numBlocksTotal) .\\Poisson3D.exe maxNumIterations generated " \
		<< "coarsestLvl[0] finestLvl[8] numCoarseSteps[64] numPreSmoothingSteps[2] numPostSmoothingSteps[2] omega[0.8]");
#	else
	LOG_NOTE("\tmpirun -np (numBlocksTotal) .\\Poisson3D.exe maxNumIterations generated");
#	endif
#else
	LOG_WARNING("Currently not fully implemented!");
#endif
}

int main (int argc, char** argv)
{
	std::vector<Fragment3DCube*>	fragments;

	MPI_Init(&argc, &argv);

	int mpiRank;
	int mpiSize;
	MPI_Comm_rank(MPI_COMM_WORLD, &mpiRank);
	MPI_Comm_size(MPI_COMM_WORLD, &mpiSize);

	// FIXME: omp_set_num_threads(OMP_NUM_THREADS);

#ifdef TIME_MEASUREMENTS
	if (argc < 9)
#else
	if (argc < 3)
#endif
	{
		if (0 == mpiRank)
			printUsage();
		MPI_Finalize();

		return 1;
	}

	unsigned int maxNumItSteps = atoi(argv[1]);

#ifdef TIME_MEASUREMENTS
	COARSE_LEVEL				= atoi(argv[3]);
	FINAL_LEVEL					= atoi(argv[4]);
	if (FINAL_LEVEL >= NUM_LEVELS)
	{
		MPI_Finalize();
		if (0 == mpiRank)
			LOG_ERROR("Finest level must not be larger than " << NUM_LEVELS-1);
		return 1;
	}

	NUM_COARSE_STEPS			= atoi(argv[5]);
	NUM_PRE_SMOOTHING_STEPS		= atoi(argv[6]);
	NUM_POST_SMOOTHING_STEPS	= atoi(argv[7]);
	OMEGA						= atof(argv[8]);
#endif

	StopWatch setupWatch;

	bool errorOccured = false;
	if ("generated" == std::string(argv[2]))
		initGeneratedDomain(fragments);
	else
		errorOccured = true;

	if (errorOccured)
	{
		if (0 == mpiRank)
			printUsage();
		MPI_Finalize();
		return 1;
	}

	std::srand(1337);

#pragma omp parallel for schedule(static, 1)
	for (int f = 0; f < fragments.size(); ++f)
	{
		for (unsigned int l = 0; l < NUM_LEVELS; ++l)
		{
			std::srand((unsigned int)fragments[f]->id);

			for (unsigned int z = 0; z < fragments[f]->solData[0][l]->numDataPointsPerDim.z; ++z)
			for (unsigned int y = 0; y < fragments[f]->solData[0][l]->numDataPointsPerDim.y; ++y)
			for (unsigned int x = 0; x < fragments[f]->solData[0][l]->numDataPointsPerDim.x; ++x)
			{
				exa_real_t val;
				if (FINAL_LEVEL != l)
					val = 0.0;
				else
				{
					val = (exa_real_t)std::rand() / RAND_MAX;

					// hack in Dirichlet boundary conditions
					Vec3u first	= Vec3u(NUM_GHOST_LAYERS);
					Vec3u last	= fragments[f]->solData[0][l]->numDataPointsPerDim - Vec3u(1) - Vec3u(NUM_GHOST_LAYERS);
					if (first.z > z || first.y > y || first.x > x || last.z < z || last.y < y || last.x < x)
						val = 0.0;
				}

				for (unsigned s = 0; s < NUM_SOL_SLOTS; ++s)
					fragments[f]->solData[s][l]->getDataRef(Vec3u(x, y, z)) = val;

				fragments[f]->rhsData[0][l]->getDataRef(Vec3u(x, y, z)) = 0.0;
				fragments[f]->resData[0][l]->getDataRef(Vec3u(x, y, z)) = 0.0;
			}
		}
	}

	MPI_Barrier(MPI_COMM_WORLD);

	exa_real_t lastRes = 0.0;
	{
		updateResidual_Node(fragments, 0, FINAL_LEVEL);
		exa_real_t res = getGlobalResidual(fragments);

		res = res * res;
		exa_real_t resTotal;

		MPI_Reduce(&res, &resTotal, 1, /*FIXME*/MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
		MPI_Bcast(&resTotal, 1, /*FIXME*/MPI_DOUBLE, 0, MPI_COMM_WORLD);

		resTotal = sqrt(resTotal);
		res = resTotal;

		lastRes = res;
	}
	exa_real_t initialRes = lastRes;

	unsigned int curIt;
	unsigned int solSlots[NUM_LEVELS];
	for (unsigned int s = 0; s < NUM_LEVELS; ++s)
		solSlots[s] = 0;

#ifdef TIME_MEASUREMENTS
	double minTime	= FLT_MAX;
	double maxTime	= 0;
	double meanTime	= 0;
	StopWatch stopWatch;
	StopWatch stopWatchTotal;
#endif

	for (curIt = 0; curIt < maxNumItSteps; ++curIt)
	{
#ifdef TIME_MEASUREMENTS
		stopWatch.reset();
#endif
		performVCycle""" + s"_${Knowledge.maxLevel}" + """(fragments, solSlots);
#ifdef TIME_MEASUREMENTS
		double tDuration = stopWatch.getTimeInMilliSecAndReset();
		minTime = std::min(minTime, tDuration);
		maxTime = std::max(maxTime, tDuration);
		meanTime += tDuration;
#endif

		// FIXME: res check every n cycles
 		if (0 != curIt % 1)
 			continue;

		exa_real_t res = getGlobalResidual(fragments);
		res = res * res;
		exa_real_t resTotal;

		MPI_Reduce(&res, &resTotal, 1, /*FIXME*/MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
		MPI_Bcast(&resTotal, 1, /*FIXME*/MPI_DOUBLE, 0, MPI_COMM_WORLD);
		
		resTotal = sqrt(resTotal);
		res = resTotal;

#		ifdef TIME_MEASUREMENTS
		if (0 == mpiRank)
		{
			LOG_NOTE("Iteration " << curIt << std::endl
				<< "\tCurrent residual (L2-norm), level " << FINAL_LEVEL << ": " << res << std::endl
				<< "\tRuntime for the current v-cycle: " << tDuration << std::endl
				<< "\tReduction: " << res / lastRes);
		}
#		else
		if (0 == mpiRank)
			LOG_NOTE("Iteration " << curIt << ", current residual (L2-norm), level " << FINAL_LEVEL << ": " << res);
#		endif

		lastRes = res;

		if (res < 1e-8 * initialRes)
		{
			++curIt;
			break;
		}
	}

#ifdef TIME_MEASUREMENTS
	for (int c = 0; c < mpiSize; ++c)
	{
		MPI_Barrier(MPI_COMM_WORLD);
		if (c == mpiRank)
		{
			double totalTime = stopWatchTotal.getTimeInMilliSecAndReset();
			/*LOG_NOTE("Parameters (coarsestLvl finestLvl numCoarseSteps numPreSmoothingSteps numPostSmoothingSteps omega:");
			LOG_NOTE(COARSE_LEVEL << " " << FINAL_LEVEL << " " << NUM_COARSE_STEPS << " " << NUM_PRE_SMOOTHING_STEPS << " " << NUM_POST_SMOOTHING_STEPS << " " << OMEGA);

			LOG_NOTE("Total Number of Cycles: " << curIt);
			LOG_NOTE("Total Execution Time:   " << totalTime.count());

			LOG_NOTE("Mean Time per Cycle:    " << (double)meanTime.count() / (double)curIt);
			LOG_NOTE("Min  Time per Cycle:    " << minTime.count());
			LOG_NOTE("Max  Time per Cycle:    " << maxTime.count());*/

			// if you need a more compact form of output:
			std::cout

#	ifdef COARSE_GRID_SOLVER_IP_SMOOTHER
				<< "IP_SMOOTHER" << "\t"
#	endif
#	ifdef COARSE_GRID_SOLVER_IP_CG
				<< "IP_CG" << "\t"
#	endif
#	ifdef COARSE_GRID_SOLVER_IP_HYPRE
				<< "IP_AMG" << "\t"
#	endif
#	ifdef COARSE_GRID_SOLVER_RED_HYPRE
				<< "RED_AMG" << "\t"
#	endif

#	ifdef SMOOTHER_JACOBI
				<< "JAC" << "\t"
#	endif
#	ifdef SMOOTHER_GS
				<< "GS" << "\t"
#	endif
#	ifdef SMOOTHER_GSAC
				<< "GSAC" << "\t"
#	endif
#	ifdef SMOOTHER_GSOD
				<< "GSOD" << "\t"
#	endif
#	ifdef SMOOTHER_GSACBE
				<< "GSACBE" << "\t"
#	endif
#	ifdef SMOOTHER_GSRS
				<< "GSRS" << "\t"
#	endif
#	ifdef SMOOTHER_GSRB
				<< "GSRB" << "\t"
#	endif
#	ifdef SMOOTHER_GSRBAC
				<< "GSRBAC" << "\t"
#	endif
/*
				FIXME: set via code generation
				<< numBlocks.x << "\t"
				<< numBlocks.y << "\t"
				<< numBlocks.z << "\t"
				<< numElemPerBlock.x << "\t"
				<< numElemPerBlock.y << "\t"
				<< numElemPerBlock.z << "\t"
*/
				<< COARSE_LEVEL << "\t"
				<< FINAL_LEVEL << "\t"
				<< NUM_COARSE_STEPS << "\t"
				<< NUM_PRE_SMOOTHING_STEPS << "\t"
				<< NUM_POST_SMOOTHING_STEPS << "\t"
				<< OMEGA << "\t";//std::endl;
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
#endif

	MPI_Barrier(MPI_COMM_WORLD);

#ifdef TIME_MEASUREMENTS
#else
#	ifdef USE_MPI
	if (0 == mpiRank)
#	endif
		LOG_NOTE("Finished after " << curIt << " iterations");
#endif

	// FIXME: free primitives
	MPI_Finalize();

	return 0;
}
""");

    writerSource.close();

  }

}
