package exastencils.application

import java.io.PrintWriter
import java.io.File

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

import exastencils.primitives.Globals

case class Poisson3D() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def printToFile = {
    val writerHeader = new PrintWriter(new File(Globals.printPath + s"Poisson3D.h"));

    writerHeader.write("""
#ifndef	POISSON_3D_H
#define	POISSON_3D_H

//=====================================================================================================================
//                                        _    __   ____   ____     ______   ____
//                                       | |  / /  /  _/  / __ \   / ____/  / __ \
//                                       | | / /   / /   / /_/ /  / __/    / /_/ /
//                                       | |/ /  _/ /   / ____/  / /___   / _, _/
//                                       |___/  /___/  /_/      /_____/  /_/ |_|
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
/// \file	Poisson3D.h
/// \brief	Header file for the Poisson3D application
/// \author	Sebastian Kuckuk
//
//=====================================================================================================================

//=====================================================================================================================
// includes
//=====================================================================================================================

#include "Util/Defines.h"				// required to set USE_* defines

#ifdef USE_MPI
#	include <mpi.h>				// required to be included before stdio.h
#endif

#include <vector>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <climits>
#include <cfloat>

#include <boost/shared_ptr.hpp>

#ifdef USE_OMP
#	include <omp.h>
#endif

#include "Util/StdIncludes.h"
#include "Util/Vector.h"
#include "Util/Stopwatch.h"

#include "MultiGrid/MultiGrid.h"

#include "Domains/DomainGenerated.h"

#endif	// POISSON_3D_H
""");

    writerHeader.close();

    val writerSource = new PrintWriter(new File(Globals.printPath + s"Poisson3D.cpp"));

    writerSource.write("""
//=====================================================================================================================
//									 _____           ____  _                  _ _     
//									| ____|_  ____ _/ ___|| |_ ___ _ __   ___(_) |___ 
//									|  _| \ \/ / _` \___ \| __/ _ \ '_ \ / __| | / __|
//									| |___ >  < (_| |___) | ||  __/ | | | (__| | \__ \
//									|_____/_/\_\__,_|____/ \__\___|_| |_|\___|_|_|___/
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
/// \file	Poisson3D.cpp
/// \brief	Source file for the Poisson3D application
/// \author	Sebastian Kuckuk
//
//=====================================================================================================================

//=====================================================================================================================
// includes
//=====================================================================================================================

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
		<< "coarsestLvl[0] finestLvl[8] numCoarseSteps[64] numPreSmoothingSteps[2] numPostSmoothingSteps[2] omega[0.8] " \
		<< "numBlocks_x numBlocks_y numBlocks_z numElementsPerBlock_x numElementsPerBlock_y numElementsPerBlock_z");
#	else
	LOG_NOTE("\tmpirun -np (numBlocksTotal) .\\Poisson3D.exe maxNumIterations generated " \
		<< "numBlocks_x numBlocks_y numBlocks_z numElementsPerBlock_x numElementsPerBlock_y numElementsPerBlock_z");
#	endif
#else
	LOG_WARNING("Currently not fully implemented!");
#endif
}

/// \brief		entry point for the application; performs necessary init steps, executes a MG Poisson and cleans up
/// \param		argc	number of given arguments
/// \param		argv	vector of given arguments
/// \returns	0 in case of success, an error code otherwise
int main (int argc, char** argv)
{
#ifdef VERBOSE
	LOG_NOTE("Starting up");
#endif

	std::vector<boost::shared_ptr<Fragment3DCube> >	fragments;

#ifdef USE_MPI
	// init MPI
	MPI_Init(&argc, &argv);
// 	int provided;
// 	MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &provided);
// 	if (provided < MPI_THREAD_MULTIPLE)
// 	{
// 		LOG_ERROR("MPI is not thread safe and only provides " << provided);
// 		MPI_Abort(MPI_COMM_WORLD, 1);
// 	}

	int	mpiRank;
	int			mpiSize;
	MPI_Comm_rank(MPI_COMM_WORLD, &mpiRank);
	MPI_Comm_size(MPI_COMM_WORLD, &mpiSize);

#	ifdef VERBOSE
	LOG_NOTE("Initializing rank " << mpiRank << " of " << mpiSize);
#	endif
#endif

#ifdef USE_OMP
// 	omp_set_num_threads(OMP_NUM_THREADS);
// #	ifdef VERBOSE
// 	LOG_NOTE("Setting number of OMP threads to " << OMP_NUM_THREADS);
// #	endif
#endif

	// init

#ifdef TIME_MEASUREMENTS
	if (argc < 9)
#else
	if (argc < 3)
#endif
	{
#ifdef USE_MPI
		if (0 == mpiRank)
			printUsage();
		MPI_Finalize();
#else
		printUsage();
#endif
		return 1;
	}

	unsigned int maxNumItSteps = atoi(argv[1]);
#	ifdef VERBOSE
	LOG_NOTE("Setting maxNumItSteps to " << maxNumItSteps);
#	endif

#ifdef TIME_MEASUREMENTS
	COARSE_LEVEL				= atoi(argv[3]);
	FINAL_LEVEL					= atoi(argv[4]);
	if (FINAL_LEVEL >= NUM_LEVELS)
	{
#	ifdef USE_MPI
		MPI_Finalize();
		if (0 == mpiRank)
#	endif
			LOG_ERROR("Finest level must not be larger than " << NUM_LEVELS-1);
		return 1;
	}

	NUM_COARSE_STEPS			= atoi(argv[5]);
	NUM_PRE_SMOOTHING_STEPS		= atoi(argv[6]);
	NUM_POST_SMOOTHING_STEPS	= atoi(argv[7]);
	OMEGA						= atof(argv[8]);

#	ifdef VERBOSE
	if (0 == mpiRank)
	{
		LOG_NOTE("Setting COARSE_LEVEL to "				<< COARSE_LEVEL);
		LOG_NOTE("Setting FINAL_LEVEL to "				<< FINAL_LEVEL);
		LOG_NOTE("Setting NUM_COARSE_STEPS to "			<< NUM_COARSE_STEPS);
		LOG_NOTE("Setting NUM_PRE_SMOOTHING_STEPS to "	<< NUM_PRE_SMOOTHING_STEPS);
		LOG_NOTE("Setting NUM_POST_SMOOTHING_STEPS to "	<< NUM_POST_SMOOTHING_STEPS);
		LOG_NOTE("Setting OMEGA to "					<< OMEGA);
		LOG_NOTE("Setting NUM_LSE_ENTRIES_PER_RANK to "	<< NUM_LSE_ENTRIES_PER_RANK);
		LOG_NOTE("Setting COARSE_GRID_RANK_STRIDE to "	<< COARSE_GRID_RANK_STRIDE);

#		ifdef COARSE_GRID_SOLVER_IP_SMOOTHER
		LOG_NOTE("Using CGS \"in place smoother\"");
#		endif
#		ifdef COARSE_GRID_SOLVER_IP_CG
		LOG_NOTE("Using CGS \"in place CG\"");
#		endif
#		ifdef COARSE_GRID_SOLVER_IP_HYPRE
		LOG_NOTE("Using CGS \"in place Hypre\\AMG\"");
#		endif
#		ifdef COARSE_GRID_SOLVER_RED_HYPRE
		LOG_NOTE("Using CGS \"reduction Hypre\\AMG\"");
#		endif

#		ifdef SMOOTHER_JACOBI
		LOG_NOTE("Using smoother \"Jacobi\"");
#		endif
#		ifdef SMOOTHER_GS
		LOG_NOTE("Using smoother \"Gauss-Seidel\"");
#		endif
#		ifdef SMOOTHER_GSAC
		LOG_NOTE("Using smoother \"Gauss-Seidel with Additional Communication\"");
#		endif
#		ifdef SMOOTHER_GSOD
		LOG_NOTE("Using smoother \"Gauss-Seidel of Death (with additional communication)\"");
#		endif
#		ifdef SMOOTHER_GSACBE
		LOG_NOTE("Using smoother \"Gauss-Seidel Block Edition with Additional Communication\"");
#		endif
#		ifdef SMOOTHER_GSRS
		LOG_NOTE("Using smoother \"Gauss-Seidel with Random Sampling\"");
#		endif
#		ifdef SMOOTHER_GSRB
		LOG_NOTE("Using smoother \"Gauss-Seidel Red-Black (RBGS)\"");
#		endif
#		ifdef SMOOTHER_GSRBAC
		LOG_NOTE("Using smoother \"Gauss-Seidel Red-Black (RBGS) with Additional Communication\"");
#		endif
	}
#	endif
#endif
	
	Vec3u numBlocks;
	Vec3u numElemPerBlock;

#ifdef USE_MPI
	StopWatch setupWatch;

	bool errorOccured = false;
	if ("generated" == std::string(argv[2]))
	{
#ifdef TIME_MEASUREMENTS
		if (argc < 15)
			errorOccured = true;
		else
		{
			numBlocks		= Vec3u(atoi(argv[9]), atoi(argv[10]), atoi(argv[11]));
			numElemPerBlock	= Vec3u(atoi(argv[12]), atoi(argv[13]), atoi(argv[14]));
#else
		if (argc < 9)
			errorOccured = true;
		else
		{
			numBlocks		= Vec3u(atoi(argv[3]), atoi(argv[4]), atoi(argv[5]));
			numElemPerBlock	= Vec3u(atoi(argv[6]), atoi(argv[7]), atoi(argv[8]));
#endif

#ifdef VERBOSE
			LOG_NOTE("Setting numBlocks to " << numBlocks);
			LOG_NOTE("Setting numElemPerBlock to " << numElemPerBlock);
#endif

#ifdef USE_OMP
// 			omp_set_num_threads(numElemPerBlock.componentProd());
// #	ifdef VERBOSE
// 			LOG_NOTE("Resetting number of OMP threads to " << numElemPerBlock.componentProd());
// #	endif
#endif

			if (numBlocks.componentProd() != mpiSize)
				errorOccured = true;
			else
				initGeneratedDomain(fragments, numBlocks, numElemPerBlock);
			/*{
				// TODO;
				fragments.push_back(boost::shared_ptr<Fragment3DCube>(new Fragment3DCube(0, Vec3(0.))));

				for (auto frag : fragments)
				{
					frag->setupBuffers();
					frag->setupCommunication();
					frag->validate();
				}
			}*/
		}
	}
	else
		errorOccured = true;

	if (errorOccured)
	{
		if (0 == mpiRank)
			printUsage();
		MPI_Finalize();
		return 1;
	}

#	ifdef VERBOSE
	if (0 == mpiRank)
		LOG_NOTE("Domain setup took " << setupWatch.getTimeInMilliSecAndReset());
#	endif
#else
	printUsage();
	return 1;
#endif

	// init values
	std::srand(1337);

#ifdef VERBOSE
	LOG_NOTE("Initializing fragment values");
#endif

#ifdef USE_OMP
#	pragma omp parallel for schedule(static, 1)
#endif
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

#ifdef USE_MPI
	MPI_Barrier(MPI_COMM_WORLD);
#endif

#ifdef VERBOSE
	LOG_NOTE("Initializing values successful");
#endif

#ifdef USE_MPI
	if (0 == mpiRank)
#endif
	{
#ifdef TIME_MEASUREMENTS
#else
		std::cout << "Init complete, push to start computation...\n";
        std::string aux;
		std::getline(std::cin, aux);
#endif
	}

#ifdef USE_MPI
	MPI_Barrier(MPI_COMM_WORLD);
#endif

	// create MG instance
	MultiGrid*	multiGrid = new MultiGrid();

#ifdef VERBOSE
	LOG_NOTE("Calculating initial residual");
#endif

	exa_real_t lastRes = 0.0;
	{
		multiGrid->updateResidual_Node(fragments, 0, FINAL_LEVEL);
		exa_real_t res = multiGrid->getGlobalResidual_Node(fragments, FINAL_LEVEL);

#ifdef USE_MPI
		res = res * res;
		exa_real_t resTotal;

		MPI_Reduce(&res, &resTotal, 1, /*FIXME*/MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
		MPI_Bcast(&resTotal, 1, /*FIXME*/MPI_DOUBLE, 0, MPI_COMM_WORLD);

		resTotal = sqrt(resTotal);
		res = resTotal;

#	ifdef VERBOSE
		if (0 == mpiRank)
			LOG_NOTE("Initial Residual: " << res);
#	endif
#else
		LOG_NOTE("Initial Residual: " << res);
#endif

		lastRes = res;
	}
	exa_real_t initialRes = lastRes;

	// run

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
#ifdef VERBOSE
		LOG_NOTE("Starting interation " << curIt);
#endif
		
#ifdef TIME_MEASUREMENTS
		stopWatch.reset();
#endif
		multiGrid->performVCycle(fragments, FINAL_LEVEL, solSlots);
#ifdef TIME_MEASUREMENTS
		double tDuration = stopWatch.getTimeInMilliSecAndReset();
		minTime = std::min(minTime, tDuration);
		maxTime = std::max(maxTime, tDuration);
		meanTime += tDuration;
#endif
		
		// check additional stuff every n iterations
 		if (0 != curIt % 1)
 			continue;

#ifdef VERBOSE
		LOG_NOTE("Updating global residual");
#endif

		// check global residual
		//std::cout << std::endl;
		//for (unsigned int l = COARSE_LEVEL; l < FINAL_LEVEL; ++l)
		//	std::cout << "Iteration " << curIt << ", current residual (L2-norm), level " << l << ": " << getGlobalResidual(vertices, edges, elements, l) << std::endl;

		exa_real_t res = multiGrid->getGlobalResidual_Node(fragments, FINAL_LEVEL);

#ifdef USE_MPI
		res = res * res;
		exa_real_t resTotal;

		MPI_Reduce(&res, &resTotal, 1, /*FIXME*/MPI_DOUBLE, MPI_SUM, 0, MPI_COMM_WORLD);
		MPI_Bcast(&resTotal, 1, /*FIXME*/MPI_DOUBLE, 0, MPI_COMM_WORLD);
		
		resTotal = sqrt(resTotal);
		res = resTotal;

//#	ifdef VERBOSE
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
//#	endif
#else
#	ifdef VERBOSE
#		ifdef TIME_MEASUREMENTS
			LOG_NOTE("Iteration " << curIt << std::endl
				<< "\tCurrent residual (L2-norm), level " << FINAL_LEVEL << ": " << res << std::endl
				<< "\tRuntime for the current v-cycle: " << tDuration << std::endl
				<< "\tReduction: " << res / lastRes);
#		else
		LOG_NOTE("Iteration " << curIt << ", current residual (L2-norm), level " << FINAL_LEVEL << ": " << res);
#		endif
#	endif
#endif

		lastRes = res;

		//if (0.0 == resTotal)
		//	break;
		if (res < 1e-8 * initialRes)
		{
			++curIt;
			break;
		}
	}

#ifdef TIME_MEASUREMENTS
#	ifdef USE_MPI
	for (int c = 0; c < mpiSize; ++c)
	{
		MPI_Barrier(MPI_COMM_WORLD);
		if (c == mpiRank)
		{
#	endif
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
				<< numBlocks.x << "\t"
				<< numBlocks.y << "\t"
				<< numBlocks.z << "\t"
				<< numElemPerBlock.x << "\t"
				<< numElemPerBlock.y << "\t"
				<< numElemPerBlock.z << "\t"
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
#	ifdef USE_MPI
		}

		if (1)		// if true, only the first process prints its measurements
			break;
	}
	
	MPI_Barrier(MPI_COMM_WORLD);
	if (0 == mpiRank)
		std::cout << std::endl;
#	endif
#endif

/*
	multiGrid->communicateSolution(fragments, FINAL_LEVEL, 0);

	for (int r = 0; r < mpiSize; ++r)
	{
		MPI_Barrier(MPI_COMM_WORLD);
		if (r == mpiRank)
			for (int f = 0; f < fragments.size(); ++f)
			{
				std::cout << "Printing element " << f << ":\n";
				for (int z = fragments[f]->getsolData(FINAL_LEVEL, 0)->numDataPointsPerDim.z - 1; z >= 0; --z)
				{
					for (int y = fragments[f]->getsolData(FINAL_LEVEL, 0)->numDataPointsPerDim.y - 1; y >= 0; --y)
					{
						for (unsigned int x = 0; x < fragments[f]->getsolData(FINAL_LEVEL, 0)->numDataPointsPerDim.x; ++x)
							std::cout << std::setw(10) << std::setprecision(6) << std::showpos << std::fixed << std::left << fragments[f]->getsolData(FINAL_LEVEL, 0)->getDataRef(Vec3u(x, y, z));
						std::cout << std::endl;
					}
					std::cout << std::endl;
				}
				std::cout << std::endl;
			}
	}
*/

#ifdef USE_MPI
	MPI_Barrier(MPI_COMM_WORLD);
#endif

#ifdef TIME_MEASUREMENTS
#else
#	ifdef USE_MPI
	if (0 == mpiRank)
#	endif
		LOG_NOTE("Finished after " << curIt << " iterations");
#endif

	SAFE_DELETE(multiGrid);

	// shutdown
#ifdef USE_MPI
	MPI_Finalize();
#endif

	return 0;
}
          """);

    writerSource.close();

  }

}
