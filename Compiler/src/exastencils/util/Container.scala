package exastencils.util

import java.io.PrintWriter
import java.io.File

import exastencils.knowledge._

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

case class Container() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def printToFile = {
    val writerHeader = new PrintWriter(new File(Globals.printPath + s"Container/Container.h"));

    writerHeader.write("""
#ifndef	CONTAINER_CONTAINER_H
#define	CONTAINER_CONTAINER_H

#include "Util/Log.h"
#include "Util/TypeDefs.h"

#include "Util/Vector.h"

class Container
{
public:
	explicit Container (const Vec3u& numDataPointsPerDim, bool addPadding = false)
		: data(0), numDataPointsWPad(0), numDataPointsPerDim(numDataPointsPerDim)
	{
		const unsigned int MULTIPLE_OF	= 8;
		const unsigned int START_OFFSET	= 1;
        
        // FIXME: incorporate padding
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

		data = new exa_real_t[numDataPointsWPad];
	}

	~Container ()
	{ SAFE_DELETE_A(data); }


	inline unsigned int getAbsIdx (const Vec3u& idx)
	{ 
		return (idx.z + firstDataPoint.z) * numDataPointsPerDimWPad.y * numDataPointsPerDimWPad.x
			+ (idx.y + firstDataPoint.y) * numDataPointsPerDimWPad.x + idx.x + firstDataPoint.x;
	}

	inline exa_real_t*	getDataPtr ()
	{ return &data[0]; }

	inline exa_real_t*	getDataPtr (const Vec3u& idx)
	{ return &data[getAbsIdx(idx)]; }

	inline exa_real_t&	getDataRef (const Vec3u& idx)
	{ return data[getAbsIdx(idx)]; }

public:
	exa_real_t*				data;

	Vec3u			numDataPointsPerDim;
	Vec3u			numDataPointsPerDimWPad;
	unsigned int	numDataPointsWPad;
	Vec3u			firstDataPoint;
	Vec3u			lastDataPoint;

};

#endif	// CONTAINER_CONTAINER_H
""");

    writerHeader.close();
  }

}
