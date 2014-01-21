package exastencils.util

import java.io.PrintWriter
import java.io.File
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.prettyprinting._

case class Container() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def printToFile = {
    val writer = PrettyprintingManager.getPrinter(s"Container/Container.h");

    writer << ("""
#ifndef	CONTAINER_CONTAINER_H
#define	CONTAINER_CONTAINER_H

#include "Util/Log.h"

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

		data = new double[numDataPointsWPad];

		x1_ = numDataPointsPerDim.x;
		x2_ = numDataPointsPerDim.y;
		x3_ = numDataPointsPerDim.z;
	}

	~Container ()
	{ SAFE_DELETE_A(data); }


	inline unsigned int getAbsIdx (const Vec3u& idx)
	{ 
		return (idx.z + firstDataPoint.z) * numDataPointsPerDimWPad.y * numDataPointsPerDimWPad.x
			+ (idx.y + firstDataPoint.y) * numDataPointsPerDimWPad.x + idx.x + firstDataPoint.x;
	}

	inline double*	getDataPtr ()
	{ return &data[0]; }

	inline double*	getDataPtr (const Vec3u& idx)
	{ return &data[getAbsIdx(idx)]; }

	inline double&	getDataRef (const Vec3u& idx)
	{ return data[getAbsIdx(idx)]; }

	inline double&	getDataRef (unsigned int idx0, unsigned int idx1, unsigned int idx2)
	{ return data[getAbsIdx(Vec3u(idx0, idx1, idx2))]; }

	inline double& operator()(unsigned int idx0, unsigned int idx1, unsigned int idx2)
	{ return getDataRef(Vec3u(idx0, idx1, idx2)); }

public:
	double*				data;

	Vec3u			numDataPointsPerDim;
	Vec3u			numDataPointsPerDimWPad;
	unsigned int	numDataPointsWPad;
	Vec3u			firstDataPoint;
	Vec3u			lastDataPoint;

	int				x1_, x2_, x3_;
};

#endif	// CONTAINER_CONTAINER_H
""");
  }
}
