package exastencils.util

import java.io.PrintWriter
import java.io.File
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.prettyprinting._

case class Stopwatch() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def printToFile = {
    val writer = PrettyprintingManager.getPrinter(s"Util/Stopwatch.h");

    writer << ("""
#ifndef	UTIL_STOPWATCH_H
#define	UTIL_STOPWATCH_H

//#define	USE_STD_CHRONO
#define	USE_GTOD

#ifdef USE_STD_CHRONO
#	include <chrono>
#endif
#ifdef USE_GTOD
#	include <sys/time.h>
#	include <sys/types.h>
#endif

class StopWatch
{
#ifdef USE_STD_CHRONO
public:
	StopWatch ()
		: lastTime(std::chrono::high_resolution_clock::now())
	{}

	~StopWatch ()
	{}

	void	reset ()
	{ lastTime = std::chrono::high_resolution_clock::now(); }

	float	getTimeInMilliSec () const
	{
		std::chrono::nanoseconds timeStep = std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::high_resolution_clock::now() - lastTime);
		return (float)(timeStep.count() / 1e6);
	}

	float	getTimeInSec () const
	{
		std::chrono::nanoseconds timeStep = std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::high_resolution_clock::now() - lastTime);
		return (float)(timeStep.count() / 1e9);
	}

	float	getTimeInSecAndReset ()
	{
		float timeStep = getTimeInSec();
		reset();
		return timeStep;
	}

	float	getTimeInMilliSecAndReset ()
	{
		float timeStep = getTimeInMilliSec();
		reset();
		return timeStep;
	}

protected:
	std::chrono::high_resolution_clock::time_point	lastTime;		///< stores the initial point of time
#endif
#ifdef USE_GTOD
public:
	StopWatch ()
	{ reset(); }

	~StopWatch ()
	{}

	void	reset ()
	{
		timeval timePoint;
		gettimeofday(&timePoint, NULL);
		lastTime = (double)(timePoint.tv_sec) * 1e3 + (double)(timePoint.tv_usec) * 1e-3;
	}

	float	getTimeInMilliSec () const
	{
		timeval timePoint;
		gettimeofday(&timePoint, NULL);
		double newTime = (double)(timePoint.tv_sec) * 1e3 + (double)(timePoint.tv_usec) * 1e-3;
		return newTime - lastTime;
	}

	float	getTimeInMilliSecAndReset ()
	{
		float timeStep = getTimeInMilliSec();
		reset();
		return timeStep;
	}

protected:
	double		lastTime;		///< stores the initial point of time
#endif
};

#endif	// UTIL_STOPWATCH_H
""");

    writer.close(); // FIXME: finalize
  }

}
