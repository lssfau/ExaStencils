package exastencils.domain

import java.io.PrintWriter
import java.io.File

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

import exastencils.primitives.Globals

case class DomainGenerated() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def printToFile = {
    val writerHeader = new PrintWriter(new File(Globals.printPath + s"Domains/DomainGenerated.h"));

    writerHeader.write("""
#ifndef DOMAINS_DOMAINGENERATED_H
#define DOMAINS_DOMAINGENERATED_H

#include <vector>
#include <map>

#include <boost/shared_ptr.hpp>

#include "Util/Defines.h"
#include "Util/Log.h"
#include "Util/TypeDefs.h"

#include "Util/Vector.h"

Vec3	minPos	= Vec3(0.);

Vec3u	numFrag;
Vec3	lenFrag	= Vec3(1.);

Vec3u	numRank;
Vec3	lenRank;

bool		isOutside (const Vec3& pos)
{
	return (pos.x < minPos.x
		|| pos.x > minPos.x + numFrag.x * lenFrag.x
		|| pos.y < minPos.y
		|| pos.y > minPos.y + numFrag.y * lenFrag.y
		|| pos.z < minPos.z
		|| pos.z > minPos.z + numFrag.z * lenFrag.z);
}

int	pos2OwnerRank (const Vec3& pos)
{
	if (isOutside(pos))
		return -1;

	Vec3 rankPos = (pos - minPos) / lenRank;
	return
		(int)floor(rankPos.z) * numRank.y * numRank.x
		+ (int)floor(rankPos.y) * numRank.x
		+ (int)floor(rankPos.x);
}

exa_id_t	pos2FragmentId (const Vec3& pos)
{
	return 
		(exa_id_t)floor((pos.z - minPos.z /*- 0.5 * lenFrag.z*/) / lenFrag.z) * numFrag.y * numFrag.x
		+ (exa_id_t)floor((pos.y - minPos.y /*- 0.5 * lenFrag.y*/) / lenFrag.y) * numFrag.x
		+ (exa_id_t)floor((pos.x - minPos.x /*- 0.5 * lenFrag.x*/) / lenFrag.x);
}

std::vector<Vec3>	rank2FragmentPositions (int mpiRank)
{
	Vec3u rankPos(mpiRank % numRank.x, (mpiRank / numRank.x) % numRank.y, mpiRank / (numRank.x * numRank.y));

	std::vector<Vec3> posVec;
	for (exa_real_t z = minPos.z + rankPos.z * lenRank.z + 0.5 * lenFrag.z; z <= minPos.z + (1 + rankPos.z) * lenRank.z - 0.5 * lenFrag.z; z += lenFrag.z)
		for (exa_real_t y = minPos.y + rankPos.y * lenRank.y + 0.5 * lenFrag.y; y <= minPos.y + (1 + rankPos.y) * lenRank.y - 0.5 * lenFrag.y; y += lenFrag.y)
			for (exa_real_t x = minPos.x + rankPos.x * lenRank.x + 0.5 * lenFrag.x; x <= minPos.x + (1 + rankPos.x) * lenRank.x - 0.5 * lenFrag.x; x += lenFrag.x)
				posVec.push_back(Vec3(x, y, z));

	return posVec;
}

void generatePrimitives (int mpiRank, std::vector<boost::shared_ptr<Fragment3DCube> >& fragments, std::map<exa_id_t, boost::shared_ptr<Fragment3DCube> >& fragmentMap)
{
	{
#ifdef USE_OMP
		std::vector<Vec3> positions = rank2FragmentPositions(mpiRank);
#	pragma omp parallel for schedule(static, 1) ordered
		for (int ompThreadId = 0; ompThreadId < (int)positions.size(); ++ompThreadId)
		{
			Vec3& pos = positions[ompThreadId];
			boost::shared_ptr<Fragment3DCube> fragment(new Fragment3DCube(pos2FragmentId(pos), pos));
#	pragma omp ordered 
			{
				fragments.push_back(fragment);
				fragmentMap[pos2FragmentId(pos)] = fragment;
			}
		}
#else
		std::vector<Vec3> positions = rank2FragmentPositions(mpiRank);
		for (auto pos = positions.begin(); pos != positions.end(); ++pos)
		{
			boost::shared_ptr<Fragment3DCube> fragment(new Fragment3DCube(pos2FragmentId(*pos), *pos));
			fragments.push_back(fragment);
			fragmentMap[pos2FragmentId(*pos)] = fragment;
		}
#endif
	}

	// set up communication
	for (std::map<exa_id_t, boost::shared_ptr<Fragment3DCube> >::iterator fragIt = fragmentMap.begin(); fragIt != fragmentMap.end(); ++fragIt)
	{
		boost::shared_ptr<Fragment3DCube> curFrag = fragIt->second;

		for (int z = -1; z <= 1; ++z)
			for (int y = -1; y <= 1; ++y)
				for (int x = -1; x <= 1; ++x)
				{
					Vec3 offset = Vec3(x, y, z);
					FRAGMENT_LOCATION loc = (FRAGMENT_LOCATION)(FRAG_CUBE_ZN_YN_XN + (z + 1) * 9 + (y + 1) * 3 + (x + 1));

					if (mpiRank == pos2OwnerRank(curFrag->pos + offset))
						curFrag->connectLocalElement(loc, fragmentMap[pos2FragmentId(curFrag->pos + offset)]);
					else if (!isOutside(curFrag->pos + offset))
						curFrag->connectRemoteElement(loc, pos2FragmentId(curFrag->pos + offset), pos2OwnerRank(curFrag->pos + offset));
				}
	}
}

void initGeneratedDomain (std::vector<boost::shared_ptr<Fragment3DCube> >& fragments, const Vec3u& numBlocks, const Vec3u& numFragmentsPerBlock)
{
#ifdef USE_MPI
	numFrag	= numBlocks * numFragmentsPerBlock;
	numRank	= numBlocks;
	lenRank	= Vec3(numFragmentsPerBlock.x, numFragmentsPerBlock.y, numFragmentsPerBlock.z);

	// setup mpi variables
	int	mpiRank;
	int			numMpiProc;
	MPI_Comm_rank(MPI_COMM_WORLD, &mpiRank);
	MPI_Comm_size(MPI_COMM_WORLD, &numMpiProc);

	if (numMpiProc != numBlocks.componentProd())
	{
		LOG_ERROR("Invalid number of MPI processes (" << numMpiProc << ") should be " << numBlocks.componentProd());
		return;
	}

#	ifdef VERBOSE
	if (0 == mpiRank)
		LOG_NOTE("Generating " << numBlocks.componentProd() << " blocks with " << numFragmentsPerBlock.componentProd() << " fragments each (" \
		<< numFrag.componentProd() << " in total)");
#	endif

	// create temp primitive maps
	std::map<exa_id_t, boost::shared_ptr<Fragment3DCube> >		fragmentMap;

	generatePrimitives(mpiRank, fragments, fragmentMap);
#else
	LOG_ERROR("Not implemented!");
#endif

	// data management
#pragma omp parallel for schedule(static, 1)
	for (int e = 0; e < fragments.size(); ++e)
		fragments[e]->setupBuffers();

	// 	FIXME: // validation
	// 	for (int e = 0; e < fragments.size(); ++e)
	// 		fragments[e]->validate();
}

#endif // DOMAINS_DOMAINGENERATED_H
""");

    writerHeader.close();
  }

}