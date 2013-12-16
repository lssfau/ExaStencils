package exastencils.util

import java.io.PrintWriter
import java.io.File

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

import exastencils.primitives.Globals

case class Container() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def printToFile = {
    val writerHeader = new PrintWriter(new File(Globals.printPath + s"Container/Container.h"));

    writerHeader.write("""
#ifndef	CONTAINER_CONTAINER_H
#define	CONTAINER_CONTAINER_H

//=====================================================================================================================
//									 _____           ____  _                  _ _     
//									| ____|_  ____ _/ ___|| |_ ___ _ __   ___(_) |___ 
//									|  _| \ \/ / _` \___ \| __/ _ \ '_ \ / __| | / __|
//									| |___ >  < (_| |___) | ||  __/ | | | (__| | \__ \
//									|_____/_/\_\__,_|____/ \__\___|_| |_|\___|_|_|___/
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
/// \file	Container.h
/// \brief	Header file for the Container class
/// \author	Sebastian Kuckuk
//
//=====================================================================================================================

//=====================================================================================================================
// includes
//=====================================================================================================================

#include "Util/StdIncludes.h"
#include "Util/Vector.h"

//=====================================================================================================================
// class
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
/// \class	Container
/// \brief	Specifies an abstract data container
/// \tparam	T			data type of the data to be managed
/// \tparam	dataLayout	layout of the data to be managed according to ::DATA_LAYOUT
/// \todo	add access functions and make members protected
//
//=====================================================================================================================
template<class T, DATA_LAYOUT dataLayout>
class Container
{
public:
	//=================================================================================================================
	// constructors / destructor
	//=================================================================================================================
	
	explicit Container (const Vec3u& numDataPointsPerDim, bool addPadding = false)
		: data(0), numDataPointsWPad(0), numDataPointsPerDim(numDataPointsPerDim)
	{
		ASSERT_ERROR((DATA_3D_CUBOID != dataLayout || numDataPointsPerDim.x == numDataPointsPerDim.y && numDataPointsPerDim.z == numDataPointsPerDim.y), "Only cubic elements are supported atm", return);
		ASSERT_ERROR((DATA_3D_CUBOID == dataLayout), "Unsupported data Layout (" << dataLayout << ") for 3D cases detected", return);
		
		// add padding

		const unsigned int MULTIPLE_OF	= 8;
		const unsigned int START_OFFSET	= 1;
#ifdef ACTIVATE_PADDING
		if (addPadding)
		{
			numDataPointsPerDimWPad	= Vec3u(((numDataPointsPerDim.x + START_OFFSET + MULTIPLE_OF - 1) / MULTIPLE_OF) * MULTIPLE_OF, numDataPointsPerDim.y, numDataPointsPerDim.z);
			firstDataPoint			= Vec3u(START_OFFSET, 0, 0);
			lastDataPoint			= numDataPointsPerDim - Vec3u(1, 1, 1) + firstDataPoint;
		}
		else
#endif
		{
			numDataPointsPerDimWPad	= numDataPointsPerDim;
			firstDataPoint			= Vec3u(0, 0, 0);
			lastDataPoint			= numDataPointsPerDim - Vec3u(1, 1, 1);
		}
		numDataPointsWPad		= numDataPointsPerDimWPad.componentProd();

		//LOG_NOTE("Setting up container with " << numDataPoints << " data points");

		data = new T[numDataPointsWPad];
	}

	~Container ()
	{
		SAFE_DELETE_A(data);
	}

	//=================================================================================================================
	// get functions
	//=================================================================================================================
	
	/// \brief		calculates the absolute index for the underlying linearized memory
	/// \param		idx		index for the element to be treated
	/// \returns	the absolute index for the element to be treated
	inline unsigned int getAbsIdx (const Vec3u& idx)
	/*{
		ASSERT_WARNING(((0 == idx.x || idx.x < numDataPointsPerDim.x) && (0 == idx.y || idx.y < numDataPointsPerDim.y) && (0 == idx.z || idx.z < numDataPointsPerDim.z)),
			"Invalid index " << idx << " for dimensions " << numDataPointsPerDim, );

		switch (dataLayout)
		{
		case DATA_2D_QUAD:	return idx.z * numDataPointsPerDim.y * numDataPointsPerDim.x + idx.y * numDataPointsPerDim.x + idx.x;
		default:
			LOG_WARNING("not implemented yet");
			return 0;
		}
	}*/
	{ 
		//if (idx.x >= lastDataPoint.x - firstDataPoint.x || idx.y >= lastDataPoint.y - firstDataPoint.y)
		//	LOG_NOTE(idx << numDataPointsPerDim << numDataPointsPerDimWPad << firstDataPoint << lastDataPoint);
		return (idx.z + firstDataPoint.z) * numDataPointsPerDimWPad.y * numDataPointsPerDimWPad.x
			+ (idx.y + firstDataPoint.y) * numDataPointsPerDimWPad.x + idx.x + firstDataPoint.x;
	}

	/// TODO
	inline T*	getDataPtr ()
	{ return &data[0]; }

	/// \brief		generates a data pointer for the element with the given index
	/// \param		idx		the index of the element to be accessed
	/// \returns	a pointer to the specified element
	inline T*	getDataPtr (const Vec3u& idx)
	{ return &data[getAbsIdx(idx)]; }

	/// \brief		generates a data reference for the element with the given index
	/// \param		idx		the index of the element to be accessed
	/// \returns	a reference to the specified element
	inline T&	getDataRef (const Vec3u& idx)
	{ return data[getAbsIdx(idx)]; }

public:
	T*				data;						///< pointer to the actual data managed

	Vec3u			numDataPointsPerDim;		///< number of 'real' data points per dimension, i.e. without padding, may also be 1D or 2D
	Vec3u			numDataPointsPerDimWPad;	///< number of data points per dimension with added padding, may also be 1D or 2D
	unsigned int	numDataPointsWPad;			///< total number of data points managed (with padding)
	Vec3u			firstDataPoint;				///< first data point per dimension (due to padding), may also be 1D or 2D
	Vec3u			lastDataPoint;				///< last data point per dimension (due to padding), may also be 1D or 2D

};

#endif	// CONTAINER_CONTAINER_H
""");

    writerHeader.close();
  }

}
