package exastencils.util

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.prettyprinting._

case class Stopwatch() extends Node with FilePrettyPrintable {
  override def printToFile : Unit = {
    if (Knowledge.testNewTimers) return // prevent overwritting of experimental timer files

    val writer = PrettyprintingManager.getPrinter(s"Util/Stopwatch.h");

    if ("MSVC" == Knowledge.targetCompiler || "GCC" == Knowledge.targetCompiler) {
      writer << ("""
#include <chrono>

class StopWatch
{
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
};
""")
    } else {
      writer << ("""
#include <sys/time.h>
#include <sys/types.h>

class StopWatch
{
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
};
""")
    }
  }
}
