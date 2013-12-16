package exastencils.multiGrid

import java.io.PrintWriter
import java.io.File

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

import exastencils.primitives.Globals

case class MultiGrid() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def printToFile = {
    val writerHeader = new PrintWriter(new File(Globals.printPath + s"MultiGrid/MultiGrid.h"));

    writerHeader.write("""
#ifndef MULTIGRID_MULTIGRID_H
#define MULTIGRID_MULTIGRID_H

//=====================================================================================================================
//									 _____           ____  _                  _ _     
//									| ____|_  ____ _/ ___|| |_ ___ _ __   ___(_) |___ 
//									|  _| \ \/ / _` \___ \| __/ _ \ '_ \ / __| | / __|
//									| |___ >  < (_| |___) | ||  __/ | | | (__| | \__ \
//									|_____/_/\_\__,_|____/ \__\___|_| |_|\___|_|_|___/
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
/// \file	MultiGrid.h
/// \brief	Summarizes various multi-grid functions
/// \author	Sebastian Kuckuk
/// \todo	Currently, the MG algorithm is specialized for a 3D Poisson problem
//
//=====================================================================================================================

//=====================================================================================================================
// includes
//=====================================================================================================================

#include "Util/Defines.h"				// required to set USE_* defines

#ifdef USE_MPI
#	include <mpi.h>						// required to be included before stdio.h
#endif

#include <vector>

#include <boost/shared_ptr.hpp>

#include "Util/StdIncludes.h"
#include "Util/Vector.h"
#include "Util/Stopwatch.h"

#include "Primitives/Fragment3DCube.h"
#include "Primitives/CommunicationFunctions.h"

/// TODO
class MultiGrid
{
public:
	//=================================================================================================================
	// constructors / destructor
	//=================================================================================================================

	/// \brief		default constructor for the MultiGrid class
	MultiGrid ();

	/// \brief		default destructor for the MultiGrid class
	~MultiGrid ();

	//=====================================================================================================================
	// smoothers & residual computations
	//=====================================================================================================================

	/// \brief		performs one smoother iteration; assumes node based data layout
	/// \param		fragments	reference to the current collection of fragments
	/// \param		targetSlot	specifies which data slot should be written to (necessary for parallel Jacobi)
	/// \param		sourceSlot	specifies which data slot should be read from (necessary for parallel Jacobi)
	/// \param		level		current multi-grid level
	/// \todo		Currently specialized to a 3D Poisson problem
	void smootherIteration_Node (std::vector<boost::shared_ptr<CurFragmentType> >& fragments, unsigned int targetSlot, unsigned int sourceSlot, unsigned int level);

	/// \brief		updates the residual for all primitives in the given collections; assumes node based data layout
	/// \param		fragments	reference to the current collection of fragments
	/// \param		slot		specifies which data slot should be read from
	/// \param		level		current multi-grid level
	/// \todo		Currently specialized to a 3D Poisson problem
	void updateResidual_Node (std::vector<boost::shared_ptr<CurFragmentType> >& fragments, unsigned int slot, unsigned int level);

	//=====================================================================================================================
	// inter-grid operations
	//=====================================================================================================================

	/// \brief		performs one restriction step using the full weighting scheme; assumes node based data layout
	/// \param		fragments	reference to the current collection of fragments
	/// \param		levelSrc	multi-grid level holding the data to be restricted
	/// \param		levelDest	multi-grid level the restricted data are to be written to
	void performRestriction_NodeFW (std::vector<boost::shared_ptr<CurFragmentType> >& fragments, unsigned int levelSrc, unsigned int levelDest);

	/// \brief		performs one prolongation (and correction) step using tri-linear interpolation; assumes node based data layout
	/// \param		fragments	reference to the current collection of fragments
	/// \param		slotSrc		specifies which data slot should be read from
	/// \param		levelSrc	multi-grid level holding the data to be prolongated
	/// \param		slotDest	specifies which data slot should be written to
	/// \param		levelDest	multi-grid level holding the data to be corrected
	void performProlongation_NodeTLI (std::vector<boost::shared_ptr<CurFragmentType> >& fragments, unsigned int slotSrc, unsigned int levelSrc, unsigned int slotDest, unsigned int levelDest);

	//=====================================================================================================================
	// core MG functions
	//=====================================================================================================================

	/// \brief		performs one full v-cycle; may be called recursively
	/// \param		fragments	reference to the current collection of fragments
	/// \param		level		current multi-grid level
	/// \param		solSlots	pointer to an array holding the current solution slots for all levels (necessary for parallel Jacobi)
	void performVCycle (std::vector<boost::shared_ptr<CurFragmentType> >& fragments, unsigned int level, unsigned int* solSlots);

	//=====================================================================================================================
	// utility functions
	//=====================================================================================================================

	/// \brief		reduces the total residual (L2-norm) of all primitives in the given collections; assumes node based data layout
	/// \param		fragments	reference to the current collection of fragments
	/// \param		level		current multi-grid level
	/// \returns	the L2-norm of the total residual for all given primitives
	exa_real_t getGlobalResidual_Node (std::vector<boost::shared_ptr<CurFragmentType> >& fragments, unsigned int level);

	/// \brief		sets the solution data for the given parameters to zero; assumes node based data layout
	/// \param		fragments	reference to the current collection of fragments
	/// \param		level		current multi-grid level
	/// \param		slot		specifies which data slot should be written to
	void setSolZero_Node (std::vector<boost::shared_ptr<CurFragmentType> >& fragments, unsigned int level, unsigned int slot);

	/// TODO
	void communicateSolution (std::vector<boost::shared_ptr<CurFragmentType> >& fragments, unsigned int level, unsigned int slot);

	/// TODO
	void communicateResidual (std::vector<boost::shared_ptr<CurFragmentType> >& fragments, unsigned int level);

protected:
#ifdef USE_MPI
	int						mpiRank;
	int						mpiSize;
#endif
};

#endif // MULTIGRID_MULTIGRID_H
""");

    writerHeader.close();

    val writerSource = new PrintWriter(new File(Globals.printPath + s"MultiGrid/MultiGrid.cpp"));

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
/// \file	MultiGrid.cpp
/// \brief	Summarizes the implementations of various multi-grid functions
/// \author	Sebastian Kuckuk
//
//=====================================================================================================================

//=====================================================================================================================
// includes
//=====================================================================================================================

#include "MultiGrid/MultiGrid.h"

//=================================================================================================================
// constructors / destructor
//=================================================================================================================

MultiGrid::MultiGrid ()
{
#ifdef USE_MPI
	MPI_Comm_rank(MPI_COMM_WORLD, &mpiRank);
	MPI_Comm_size(MPI_COMM_WORLD, &mpiSize);
#endif
}

MultiGrid::~MultiGrid ()
{}

//=====================================================================================================================
// smoothers & residual computations
//=====================================================================================================================

void MultiGrid::smootherIteration_Node (std::vector<boost::shared_ptr<CurFragmentType> >& fragments, unsigned int targetSlot, unsigned int sourceSlot, unsigned int level)
{
	exa_real_t h = 1.0;// / (1u << level);
	exa_real_t hSq = h * h;
	exa_real_t hSqInv = 1.0 / hSq;

	communicateSolution(fragments, level, sourceSlot);

#ifdef SMOOTHER_GSOD
	for (unsigned int smootherIteration = 0; smootherIteration < NUM_GSOD_ITERATIONS; ++smootherIteration)
	{
#endif

	// solver iteration (elements)
#ifdef USE_OMP
#	pragma omp parallel for schedule(static, 1)
#endif
	for (int f = 0; f < fragments.size(); ++f)	// unsigned wont work... really, OMP?
	{
		// more specialized version; usage of different data layouts (2D vs 3D, quad vs triangle, etc) would have to be incoporated by hand or generation
		const exa_real_t*	solSrcData	= fragments[f]->solData[sourceSlot][level]->getDataPtr();
		exa_real_t*			solDestData	= fragments[f]->solData[targetSlot][level]->getDataPtr();
		const exa_real_t*	rhsData		= fragments[f]->rhsData[0][level]->getDataPtr();
		Vec3u				dimWOPad	= fragments[f]->solData[0][level]->numDataPointsPerDim;
		Vec3u				dimWPad		= fragments[f]->solData[0][level]->numDataPointsPerDimWPad;
		Vec3u				first		= fragments[f]->solData[0][level]->firstDataPoint;
		Vec3u				last		= fragments[f]->solData[0][level]->lastDataPoint;

#ifdef SMOOTHER_GSACBE
		Vec3u				numBlocks	= (dimWOPad - Vec3u(2 * NUM_GHOST_LAYERS) + Vec3u(GSBE_WINDOW_SIZE - GSBE_WINDOW_OVERLAP - 1)) / Vec3u(GSBE_WINDOW_SIZE - GSBE_WINDOW_OVERLAP) + Vec3u(1);

// 		if (0 == mpiRank && 0 == e)
// 			LOG_NOTE(numBlocks);

		for (unsigned int bz = 0; bz < numBlocks.z; ++bz)
		for (unsigned int by = 0; by < numBlocks.y; ++by)
		for (unsigned int bx = 0; bx < numBlocks.x; ++bx)
		{
			unsigned int zMin = NUM_GHOST_LAYERS + bz * (GSBE_WINDOW_SIZE - GSBE_WINDOW_OVERLAP);
			unsigned int zMax = std::min(zMin + GSBE_WINDOW_SIZE, dimWOPad.z - NUM_GHOST_LAYERS);
			unsigned int yMin = NUM_GHOST_LAYERS + by * (GSBE_WINDOW_SIZE - GSBE_WINDOW_OVERLAP);
			unsigned int yMax = std::min(yMin + GSBE_WINDOW_SIZE, dimWOPad.y - NUM_GHOST_LAYERS);
			unsigned int xMin = NUM_GHOST_LAYERS + bx * (GSBE_WINDOW_SIZE - GSBE_WINDOW_OVERLAP);
			unsigned int xMax = std::min(xMin + GSBE_WINDOW_SIZE, dimWOPad.x - NUM_GHOST_LAYERS);

			for (unsigned int i = 0; i < NUM_GSBE_ITERATIONS; ++i)
			{
				for (unsigned int z = zMin + first.z; z < zMax + first.z; ++z)
				for (unsigned int y = yMin + first.y; y < yMax + first.y; ++y)
				for (unsigned int x = xMin + first.x; x < xMax + first.x; ++x)
				{
					unsigned int posAbs = z * dimWPad.xy().componentProd() + y * dimWPad.x + x;
					solDestData[posAbs] = 
						(1.0 - OMEGA) * solSrcData[posAbs]
						+ OMEGA * 1.0 / 6.0 * (
								solSrcData[posAbs - 1] + solSrcData[posAbs + 1]
							+ solSrcData[posAbs - dimWPad.x] + solSrcData[posAbs + dimWPad.x]
							+ solSrcData[posAbs - dimWPad.x * dimWPad.y] + solSrcData[posAbs + dimWPad.x * dimWPad.y]
							- rhsData[posAbs]);
				}
			}
		}
#elif defined(SMOOTHER_GSRS)
// 		unsigned int numSamples = (dim.x - 2) * (dim.y - 2);
// 		unsigned int* samples = new unsigned int[numSamples];
// 
// 		unsigned int s = 0;
// 		for (unsigned int y = 1; y < dim.y - 1; ++y)
// 			for (unsigned int x = 1; x < dim.x - 1; ++x, ++s)
// 				samples[s] = y * dim.x + x;
// 
// 		for (unsigned int r = 0; r < 32 * 1024; ++r)
// 			std::swap(samples[std::rand() % numSamples], samples[std::rand() % numSamples]);
// 
// 		for (unsigned int s = 0; s < numSamples; ++s)
// 		{
// 			unsigned int posAbs = samples[s];
// 
// 			solDestData[posAbs] = 
// 				(1.0 - OMEGA) * solSrcData[posAbs]
// 				+ OMEGA * 0.25 * (solSrcData[posAbs - 1] + solSrcData[posAbs + 1] + solSrcData[posAbs - dim.x] + solSrcData[posAbs + dim.x] - rhsData[posAbs]);
// 		}

		for (unsigned int randCnt = 0; randCnt < 2 * dimWOPad.componentProd(); ++randCnt)
		{
			unsigned int x = (std::rand() % (dimWOPad.x - 2)) + 1 + first.x;
			unsigned int y = (std::rand() % (dimWOPad.y - 2)) + 1 + first.y;
			unsigned int posAbs = y * dimWPad.x + x;

			solDestData[posAbs] = 
				(1.0 - OMEGA) * solSrcData[posAbs]
				+ OMEGA * 0.25 * (solSrcData[posAbs - 1] + solSrcData[posAbs + 1] + solSrcData[posAbs - dimWPad.x] + solSrcData[posAbs + dimWPad.x] - rhsData[posAbs]);
		}
#elif defined (SMOOTHER_GSRB) || defined (SMOOTHER_GSRBAC)
		for (unsigned int z = first.z + NUM_GHOST_LAYERS; z <= last.z - NUM_GHOST_LAYERS; ++z)
		{
			for (unsigned int y = first.y + NUM_GHOST_LAYERS; y <= last.y - NUM_GHOST_LAYERS; ++y)
			{
				unsigned int posAbs = z * dimWPad.y * dimWPad.x + y * dimWPad.x + first.x + NUM_GHOST_LAYERS + ((y - first.y - NUM_GHOST_LAYERS) % 2);
				for (unsigned int x = first.x + NUM_GHOST_LAYERS + ((y - first.y - NUM_GHOST_LAYERS) % 2); x <= last.x - NUM_GHOST_LAYERS; x += 2, posAbs += 2)
				{
					solDestData[posAbs] = 
						(1.0 - OMEGA) * solSrcData[posAbs]
						+ OMEGA * 1.0 / 6.0 * (
							  solSrcData[posAbs - 1] + solSrcData[posAbs + 1]
							+ solSrcData[posAbs - dimWPad.x] + solSrcData[posAbs + dimWPad.x]
							+ solSrcData[posAbs - dimWPad.x * dimWPad.y] + solSrcData[posAbs + dimWPad.x * dimWPad.y]
							- rhsData[posAbs]);
				}
			}
		}

#	ifdef USE_ADDITIONAL_SMOOTHER_COMM
		communicateSolution(fragments, level, targetSlot);
#	endif

		for (unsigned int z = first.z + NUM_GHOST_LAYERS; z <= last.z - NUM_GHOST_LAYERS; ++z)
		{
			for (unsigned int y = first.y + NUM_GHOST_LAYERS; y <= last.y - NUM_GHOST_LAYERS; ++y)
			{
				unsigned int posAbs = z * dimWPad.y * dimWPad.x + y * dimWPad.x + first.x + NUM_GHOST_LAYERS + ((y - first.y - NUM_GHOST_LAYERS + 1) % 2);
				for (unsigned int x = first.x + NUM_GHOST_LAYERS + ((y - first.y - NUM_GHOST_LAYERS + 1) % 2); x <= last.x - NUM_GHOST_LAYERS; x += 2, posAbs += 2)
				{
					solDestData[posAbs] = 
						(1.0 - OMEGA) * solSrcData[posAbs]
						+ OMEGA * 1.0 / 6.0 * (
							  solSrcData[posAbs - 1] + solSrcData[posAbs + 1]
							+ solSrcData[posAbs - dimWPad.x] + solSrcData[posAbs + dimWPad.x]
							+ solSrcData[posAbs - dimWPad.x * dimWPad.y] + solSrcData[posAbs + dimWPad.x * dimWPad.y]
							- rhsData[posAbs]);
				}
			}
		}
#else
		for (unsigned int z = first.z + NUM_GHOST_LAYERS; z <= last.z - NUM_GHOST_LAYERS; ++z)
		{
			for (unsigned int y = first.y + NUM_GHOST_LAYERS; y <= last.y - NUM_GHOST_LAYERS; ++y)
			{
				unsigned int posAbs = z * dimWPad.y * dimWPad.x + y * dimWPad.x + first.x + NUM_GHOST_LAYERS;
				for (unsigned int x = first.x + NUM_GHOST_LAYERS; x <= last.x - NUM_GHOST_LAYERS; ++x, ++posAbs)
				{
					solDestData[posAbs] = 
						(1.0 - OMEGA) * solSrcData[posAbs]
						+ OMEGA * 1.0 / 6.0 * (
							  solSrcData[posAbs - 1] + solSrcData[posAbs + 1]
							+ solSrcData[posAbs - dimWPad.x] + solSrcData[posAbs + dimWPad.x]
							+ solSrcData[posAbs - dimWPad.x * dimWPad.y] + solSrcData[posAbs + dimWPad.x * dimWPad.y]
							- rhsData[posAbs]);
				}
			}
		}
#endif
	}

#ifdef SMOOTHER_GSOD
	}
#endif
}

void MultiGrid::updateResidual_Node (std::vector<boost::shared_ptr<CurFragmentType> >& fragments, unsigned int slot, unsigned int level)
{
	exa_real_t h = 1.0;// / (1u << level);
	exa_real_t hSq = h * h;
	exa_real_t hSqInv = 1.0 / hSq;

	communicateSolution(fragments, level, slot);

#ifdef USE_OMP
#	pragma omp parallel for schedule(static, 1)
#endif
	for (int f = 0; f < fragments.size(); ++f)	// unsigned wont work... really, OMP?
	{
		// more specialized version; usage of different data layouts (2D vs 3D, quad vs triangle, etc) would have to be incoporated by hand or generation
		const exa_real_t*	solData	= fragments[f]->solData[slot][level]->getDataPtr();
		exa_real_t*			resData	= fragments[f]->resData[0][level]->getDataPtr();
		const exa_real_t*	rhsData	= fragments[f]->rhsData[0][level]->getDataPtr();
		Vec3u				dimWPad	= fragments[f]->solData[0][level]->numDataPointsPerDimWPad;
		Vec3u				first	= fragments[f]->solData[0][level]->firstDataPoint;
		Vec3u				last	= fragments[f]->solData[0][level]->lastDataPoint;

		for (unsigned int z = first.z + NUM_GHOST_LAYERS; z <= last.z - NUM_GHOST_LAYERS; ++z)
		for (unsigned int y = first.y + NUM_GHOST_LAYERS; y <= last.y - NUM_GHOST_LAYERS; ++y)
		{
			unsigned int posAbs = z * dimWPad.y * dimWPad.x + y * dimWPad.x + first.x + NUM_GHOST_LAYERS;
			for (unsigned int x = first.x + NUM_GHOST_LAYERS; x <= last.x - NUM_GHOST_LAYERS; ++x, ++posAbs)
				resData[posAbs] =
					rhsData[posAbs]
					- solData[posAbs - 1] - solData[posAbs + 1]
					- solData[posAbs - dimWPad.x] - solData[posAbs + dimWPad.x]
					- solData[posAbs - dimWPad.y * dimWPad.x] - solData[posAbs + dimWPad.y * dimWPad.x]
					+ 6.0 * solData[posAbs];
		}
	}
}

//=====================================================================================================================
// inter-grid operations
//=====================================================================================================================

void MultiGrid::performRestriction_NodeFW (std::vector<boost::shared_ptr<CurFragmentType> >& fragments, unsigned int levelSrc, unsigned int levelDest)
{
	communicateResidual(fragments, levelSrc);

#ifdef USE_OMP
#	pragma omp parallel for schedule(static, 1)
#endif
	for (int f = 0; f < fragments.size(); ++f)
	{
		// more specialized version; usage of different data layouts (2D vs 3D, quad vs triangle, etc) would have to be incoporated by hand or generation
		const exa_real_t*	srcData		= fragments[f]->resData[0][levelSrc]->getDataPtr();
		exa_real_t*			destData	= fragments[f]->rhsData[0][levelDest]->getDataPtr();
		Vec3u				dimWPadSrc	= fragments[f]->solData[0][levelSrc]->numDataPointsPerDimWPad;
		Vec3u				firstSrc	= fragments[f]->solData[0][levelSrc]->firstDataPoint;
		Vec3u				lastSrc		= fragments[f]->solData[0][levelSrc]->lastDataPoint;
		Vec3u				dimWPadDest	= fragments[f]->solData[0][levelDest]->numDataPointsPerDimWPad;
		Vec3u				firstDest	= fragments[f]->solData[0][levelDest]->firstDataPoint;
		Vec3u				lastDest	= fragments[f]->solData[0][levelDest]->lastDataPoint;

		unsigned int		dxS = 1;
		unsigned int		dyS	= dimWPadSrc.x;
		unsigned int		dzS	= dimWPadSrc.x * dimWPadSrc.y;

		for (unsigned int z = firstDest.z + NUM_GHOST_LAYERS; z <= lastDest.z - NUM_GHOST_LAYERS; ++z)
		{
			for (unsigned int y = firstDest.y + NUM_GHOST_LAYERS; y <= lastDest.y - NUM_GHOST_LAYERS; ++y)
			{
				unsigned int posAbs = (2 * (z - firstDest.z - NUM_GHOST_LAYERS) + firstSrc.z + NUM_GHOST_LAYERS) * dzS
					+ (2 * (y - firstDest.y - NUM_GHOST_LAYERS) + firstSrc.y + NUM_GHOST_LAYERS) * dyS
					+ firstSrc.x + NUM_GHOST_LAYERS;
				for (unsigned int x = firstDest.x + NUM_GHOST_LAYERS; x <= lastDest.x - NUM_GHOST_LAYERS; ++x, posAbs += 2)
				{
					destData[z * dimWPadDest.y * dimWPadDest.x + y * dimWPadDest.x + x] =
						+ 1.0 * (
							  srcData[posAbs])						// 1
						+ 0.5 * (
							  srcData[posAbs - dxS]
							+ srcData[posAbs + dxS]
							+ srcData[posAbs - dyS]
							+ srcData[posAbs + dyS]
							+ srcData[posAbs - dzS]
							+ srcData[posAbs + dzS])				// 3
						+ 0.25 * (
							  srcData[posAbs - dyS - dxS]
							+ srcData[posAbs - dyS + dxS]
							+ srcData[posAbs + dyS - dxS]
							+ srcData[posAbs + dyS + dxS]
							+ srcData[posAbs - dzS - dxS]
							+ srcData[posAbs - dzS + dxS]
							+ srcData[posAbs + dzS - dxS]
							+ srcData[posAbs + dzS + dxS]
							+ srcData[posAbs - dzS - dyS]
							+ srcData[posAbs - dzS + dyS]
							+ srcData[posAbs + dzS - dyS]
							+ srcData[posAbs + dzS + dyS])			// 3
						+ 0.125 * (
							  srcData[posAbs - dzS - dyS - dxS]
							+ srcData[posAbs - dzS - dyS + dxS]
							+ srcData[posAbs - dzS + dyS - dxS]
							+ srcData[posAbs - dzS + dyS + dxS]
							+ srcData[posAbs + dzS - dyS - dxS]
							+ srcData[posAbs + dzS - dyS + dxS]
							+ srcData[posAbs + dzS + dyS - dxS]
							+ srcData[posAbs + dzS + dyS + dxS]);	// 1
					destData[z * dimWPadDest.x * dimWPadDest.y + y * dimWPadDest.x + x] *= 0.5;
				}
			}
		}
	}
}

void MultiGrid::performProlongation_NodeTLI (std::vector<boost::shared_ptr<CurFragmentType> >& fragments, unsigned int slotSrc, unsigned int levelSrc, unsigned int slotDest, unsigned int levelDest)
{
	communicateSolution(fragments, levelSrc, slotSrc);

#ifdef USE_OMP
#	pragma omp parallel for schedule(static, 1)
#endif
	for (int f = 0; f < fragments.size(); ++f)
	{
		// more specialized version; usage of different data layouts (2D vs 3D, quad vs triangle, etc) would have to be incoporated by hand or generation
		// WARNING: assumes an odd number of data points per dimension (e.g. 2^l + 1) or a number smaller or equal to 2
		const exa_real_t*	srcData		= fragments[f]->solData[slotSrc][levelSrc]->getDataPtr();
		exa_real_t*			destData	= fragments[f]->solData[slotDest][levelDest]->getDataPtr();
		Vec3u				dimWPadSrc	= fragments[f]->solData[0][levelSrc]->numDataPointsPerDimWPad;
		Vec3u				firstSrc	= fragments[f]->solData[0][levelSrc]->firstDataPoint;
		Vec3u				lastSrc		= fragments[f]->solData[0][levelSrc]->lastDataPoint;
		Vec3u				dimWPadDest	= fragments[f]->solData[0][levelDest]->numDataPointsPerDimWPad;
		Vec3u				firstDest	= fragments[f]->solData[0][levelDest]->firstDataPoint;
		Vec3u				lastDest	= fragments[f]->solData[0][levelDest]->lastDataPoint;

		unsigned int		dxS = 1;
		unsigned int		dyS	= dimWPadSrc.x;
		unsigned int		dzS	= dimWPadSrc.x * dimWPadSrc.y;
		unsigned int		dxD = 1;
		unsigned int		dyD	= dimWPadDest.x;
		unsigned int		dzD	= dimWPadDest.x * dimWPadDest.y;

		for (unsigned int z = firstDest.z + NUM_GHOST_LAYERS; z <= lastDest.z - NUM_GHOST_LAYERS; z += 2)
		{
			for (unsigned int y = firstDest.y + NUM_GHOST_LAYERS; y <= lastDest.y - NUM_GHOST_LAYERS; y += 2)
			{
				unsigned int destPos = z * dzD + y * dyD + firstDest.x + NUM_GHOST_LAYERS;
				unsigned int srcPos = (((z - firstDest.z - NUM_GHOST_LAYERS) >> 1) + firstSrc.z + NUM_GHOST_LAYERS) * dzS
					+ (((y - firstDest.y - NUM_GHOST_LAYERS) >> 1) + firstSrc.y + NUM_GHOST_LAYERS) * dyS
					+ firstSrc.x + NUM_GHOST_LAYERS;
				for (unsigned int x = firstDest.x + NUM_GHOST_LAYERS; x <= lastDest.x - NUM_GHOST_LAYERS; x += 2, destPos += 2, ++srcPos)
				{
					// TODO: extract if condition

					destData[destPos] += 1.0 * (
						  srcData[srcPos]);

					if (x < lastDest.x - NUM_GHOST_LAYERS)
					destData[destPos + dxD] += 0.5 * (
						  srcData[srcPos]
						+ srcData[srcPos + dxS]);

					if (y < lastDest.y - NUM_GHOST_LAYERS)
					destData[destPos + dyD] += 0.5 * (
						  srcData[srcPos]
						+ srcData[srcPos + dyS]);

					if (z < lastDest.z - NUM_GHOST_LAYERS)
					destData[destPos + dzD] += 0.5 * (
						  srcData[srcPos]
						+ srcData[srcPos + dzS]);

					if (x < lastDest.x - NUM_GHOST_LAYERS && y < lastDest.y - NUM_GHOST_LAYERS)
					destData[destPos + dyD + dxD] += 0.25 * (
						  srcData[srcPos]
						+ srcData[srcPos + dxS]
						+ srcData[srcPos + dyS]
						+ srcData[srcPos + dyS + dxS]);

					if (x < lastDest.x - NUM_GHOST_LAYERS && z < lastDest.z - NUM_GHOST_LAYERS)
					destData[destPos + dzD + dxD] += 0.25 * (
						  srcData[srcPos]
						+ srcData[srcPos + dxS]
						+ srcData[srcPos + dzS]
						+ srcData[srcPos + dzS + dxS]);

					if (y < lastDest.y - NUM_GHOST_LAYERS && z < lastDest.z - NUM_GHOST_LAYERS)
					destData[destPos + dzD + dyD] += 0.25 * (
						  srcData[srcPos]
						+ srcData[srcPos + dyS]
						+ srcData[srcPos + dzS]
						+ srcData[srcPos + dzS + dyS]);

					if (x < lastDest.x - NUM_GHOST_LAYERS && y < lastDest.y - NUM_GHOST_LAYERS && z < lastDest.z - NUM_GHOST_LAYERS)
					destData[destPos + dzD + dyD + dxD] += 0.125 * (
						  srcData[srcPos]
						+ srcData[srcPos + dxS]
						+ srcData[srcPos + dyS]
						+ srcData[srcPos + dyS + dxS]
						+ srcData[srcPos + dzS]
						+ srcData[srcPos + dzS + dxS]
						+ srcData[srcPos + dzS + dyS]
						+ srcData[srcPos + dzS + dyS + dxS]);
				}
			}
		}
	}
}

//=====================================================================================================================
// core MG functions
//=====================================================================================================================

void MultiGrid::performVCycle (std::vector<boost::shared_ptr<CurFragmentType> >& fragments, unsigned int level, unsigned int* solSlots)
{
#ifdef MEASURE_MG_COMPONENTS
	StopWatch	stopWatch;
#endif

	// check for coarsest level
	if (COARSE_LEVEL == level)
	{
#ifdef COARSE_GRID_SOLVER_IP_SMOOTHER
		for (unsigned int s = 0; s < NUM_COARSE_STEPS; ++s)
		{
			++solSlots[COARSE_LEVEL];
			smootherIteration_Node(fragments, (solSlots[level] + 0) % NUM_SOL_SLOTS, (solSlots[level] - 1) % NUM_SOL_SLOTS, level);
		}
#endif

#ifdef MEASURE_MG_COMPONENTS
		if (0 == mpiRank)
			LOG_NOTE("Coarse grid solving on level " << level << " required " << stopWatch.getTimeInMilliSecAndReset());
#endif

		if (FINAL_LEVEL == COARSE_LEVEL)	// no real MG, just simple Jacobi
			updateResidual_Node(fragments, solSlots[level] % NUM_SOL_SLOTS, level);

		return;
	}

	// not coarsest level, perform default MG step

#ifdef MEASURE_MG_COMPONENTS
	stopWatch.reset();
#endif

	// pre-smoothing
	for (unsigned int s = 0; s < NUM_PRE_SMOOTHING_STEPS; ++s)
	{
		++solSlots[level];
		smootherIteration_Node(fragments, (solSlots[level] + 0) % NUM_SOL_SLOTS, (solSlots[level] - 1) % NUM_SOL_SLOTS, level);
	}

#ifdef MEASURE_MG_COMPONENTS
	if (0 == mpiRank)
		LOG_NOTE("Pre-Smoothing on level " << level << " required " << stopWatch.getTimeInMilliSecAndReset());
#endif

	// residual calculation
	updateResidual_Node(fragments, solSlots[level] % NUM_SOL_SLOTS, level);

#ifdef MEASURE_MG_COMPONENTS
	if (0 == mpiRank)
		LOG_NOTE("Updating the residual on level " << level << " required " << stopWatch.getTimeInMilliSecAndReset());
#endif

	// restriction
	performRestriction_NodeFW(fragments, level, level - 1);

#ifdef MEASURE_MG_COMPONENTS
	if (0 == mpiRank)
		LOG_NOTE("Restricting on level " << level << " required " << stopWatch.getTimeInMilliSecAndReset());
#endif

	// reset solution of coarser grid
	setSolZero_Node(fragments, level - 1, solSlots[level - 1] % NUM_SOL_SLOTS);

#ifdef MEASURE_MG_COMPONENTS
	if (0 == mpiRank)
		LOG_NOTE("Resetting the solution on level " << level - 1 << " required " << stopWatch.getTimeInMilliSecAndReset());
#endif

	//LOG_NOTE("Starting recursion from level " << level << " with " << elements[0]->getNumDataPointsPerDim(level) << " points");

	// solve on coarse grid
	performVCycle(fragments, level - 1, solSlots);

	//LOG_NOTE("Resuming from recursion at level " << level << " with " << elements[0]->getNumDataPointsPerDim(level) << " points");

#ifdef MEASURE_MG_COMPONENTS
	stopWatch.reset();
#endif

	// prolongation & correction
	performProlongation_NodeTLI(fragments, solSlots[level - 1] % NUM_SOL_SLOTS, level - 1, solSlots[level] % NUM_SOL_SLOTS, level);

#ifdef MEASURE_MG_COMPONENTS
	if (0 == mpiRank)
		LOG_NOTE("Prolongation on level " << level << " required " << stopWatch.getTimeInMilliSecAndReset());
#endif

	// post-smoothing
	for (unsigned int s = 0; s < NUM_POST_SMOOTHING_STEPS; ++s)
	{
		++solSlots[level];
		smootherIteration_Node(fragments, (solSlots[level] + 0) % NUM_SOL_SLOTS, (solSlots[level] - 1) % NUM_SOL_SLOTS, level);
	}

#ifdef MEASURE_MG_COMPONENTS
	if (0 == mpiRank)
		LOG_NOTE("Post-Smoothing on level " << level << " required " << stopWatch.getTimeInMilliSecAndReset());
#endif

	// prepare res for reduction on finest level
	if (FINAL_LEVEL == level)
		updateResidual_Node(fragments, solSlots[level] % NUM_SOL_SLOTS, level);
}

//=====================================================================================================================
// utility functions
//=====================================================================================================================

exa_real_t MultiGrid::getGlobalResidual_Node (std::vector<boost::shared_ptr<CurFragmentType> >& fragments, unsigned int level)
{
	exa_real_t res = 0.0;

#ifdef USE_OMP
#	pragma omp parallel for reduction(+:res) schedule(static, 1)
#endif
	for (int f = 0; f < fragments.size(); ++f)
	{
		// more specialized version; usage of different data layouts (2D vs 3D, quad vs triangle, etc) would have to be incoporated by hand or generation
		const exa_real_t*	data	= fragments[f]->resData[0][level]->getDataPtr();
		Vec3u				dimWPad	= fragments[f]->solData[0][level]->numDataPointsPerDimWPad;
		Vec3u				first	= fragments[f]->solData[0][level]->firstDataPoint;
		Vec3u				last	= fragments[f]->solData[0][level]->lastDataPoint;

		for (unsigned int z = first.z + NUM_GHOST_LAYERS; z <= last.z - NUM_GHOST_LAYERS; ++z)
		{
			for (unsigned int y = first.y + NUM_GHOST_LAYERS; y <= last.y - NUM_GHOST_LAYERS; ++y)
			{
				unsigned int posAbs = z * dimWPad.y * dimWPad.x + y * dimWPad.x + first.x + NUM_GHOST_LAYERS;
				for (unsigned int x = first.x + NUM_GHOST_LAYERS; x <= last.x - NUM_GHOST_LAYERS; ++x, ++posAbs)
					res += data[posAbs] * data[posAbs];
			}
		}
	}

	return sqrt(res);
}

void MultiGrid::setSolZero_Node (std::vector<boost::shared_ptr<CurFragmentType> >& fragments, unsigned int level, unsigned int slot)
{
#ifdef USE_OMP
#	pragma omp parallel for schedule(static, 1)
#endif
	for (int e = 0; e < fragments.size(); ++e)
	{
		// more specialized version; usage of different data layouts (2D vs 3D, quad vs triangle, etc) would have to be incoporated by hand or generation
		exa_real_t*			data	= fragments[e]->solData[slot][level]->getDataPtr();
		Vec3u				dimWPad	= fragments[e]->solData[0][level]->numDataPointsPerDimWPad;
		Vec3u				first	= fragments[e]->solData[0][level]->firstDataPoint;
		Vec3u				last	= fragments[e]->solData[0][level]->lastDataPoint;

		for (unsigned int z = first.z + NUM_GHOST_LAYERS; z <= last.z - NUM_GHOST_LAYERS; ++z)
		{
			for (unsigned int y = first.y + NUM_GHOST_LAYERS; y <= last.y - NUM_GHOST_LAYERS; ++y)
			{
				unsigned int posAbs = z * dimWPad.y * dimWPad.x + y * dimWPad.x + first.x + NUM_GHOST_LAYERS;
				for (unsigned int x = first.x + NUM_GHOST_LAYERS; x <= last.x - NUM_GHOST_LAYERS; ++x, ++posAbs)
					data[posAbs] = 0.0;
			}
		}
	}
}

void MultiGrid::communicateSolution (std::vector<boost::shared_ptr<CurFragmentType> >& fragments, unsigned int level, unsigned int slot)
{
// 	for (int e = 0; e < fragments.size(); ++e)
// 		fragments[e]->exchsolDataX(level, slot);
// 	for (int e = 0; e < fragments.size(); ++e)
// 		fragments[e]->exchsolDataY(level, slot);
// 	for (int e = 0; e < fragments.size(); ++e)
// 		fragments[e]->exchsolDataZ(level, slot);

// 	for (int e = 0; e < fragments.size(); ++e)
// 		fragments[e]->exchsolData(level, slot);

	exchsolData(fragments, level, slot);
}

void MultiGrid::communicateResidual(std::vector<boost::shared_ptr<CurFragmentType> >& fragments, unsigned int level)
{
// 	for (int e = 0; e < fragments.size(); ++e)
// 		fragments[e]->exchresDataX(level);
// 	for (int e = 0; e < fragments.size(); ++e)
// 		fragments[e]->exchresDataY(level);
// 	for (int e = 0; e < fragments.size(); ++e)
// 		fragments[e]->exchresDataZ(level);

// 	for (int e = 0; e < fragments.size(); ++e)
// 		fragments[e]->exchresData(level);

	exchresData(fragments, level, 0);
}
          """);

    writerSource.close();

  }

}
