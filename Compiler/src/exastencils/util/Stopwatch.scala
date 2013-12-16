package exastencils.util

import java.io.PrintWriter
import java.io.File

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

import exastencils.primitives.Globals

case class Stopwatch() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def printToFile = {
    val writerHeader = new PrintWriter(new File(Globals.printPath + s"Util/Stopwatch.h"));

    writerHeader.write("""
#ifndef	UTIL_STOPWATCH_H
#define	UTIL_STOPWATCH_H

//=====================================================================================================================
//									 _____           ____  _                  _ _     
//									| ____|_  ____ _/ ___|| |_ ___ _ __   ___(_) |___ 
//									|  _| \ \/ / _` \___ \| __/ _ \ '_ \ / __| | / __|
//									| |___ >  < (_| |___) | ||  __/ | | | (__| | \__ \
//									|_____/_/\_\__,_|____/ \__\___|_| |_|\___|_|_|___/
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
/// \file	Stopwatch.h
/// \brief	Header file for the Stopwatch class
/// \author	Sebastian Kuckuk
//
//=====================================================================================================================

//=====================================================================================================================
// defines
//=====================================================================================================================

#define	USE_STD_CHRONO
//#define	USE_GTOD

//=====================================================================================================================
// includes
//=====================================================================================================================

#ifdef USE_STD_CHRONO
#	include <chrono>
#endif
#ifdef USE_GTOD
#	include <sys/time.h>
#	include <sys/types.h>
#endif

//=====================================================================================================================
// class
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
/// \class	StopWatch
/// \brief	provides a simple stopwatch functionality
//
//=====================================================================================================================
class StopWatch
{
#ifdef USE_STD_CHRONO
public:
	/// \brief		default constructor for the StopWatch class
	StopWatch ()
		: lastTime(std::chrono::high_resolution_clock::now())
	{}

	/// \brief		default destructor for the StopWatch class
	~StopWatch ()
	{}

	//=================================================================================================================
	// basic functions
	//=================================================================================================================

	/// \brief		resets the stopwatch
	void	reset ()
	{ lastTime = std::chrono::high_resolution_clock::now(); }
	
	/// \brief		gets the current time of the stopwatch
	/// \returns	the current time in milliseconds
	float	getTimeInMilliSec () const
	{
		std::chrono::nanoseconds timeStep = std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::high_resolution_clock::now() - lastTime);
		return (float)(timeStep.count() / 1e6);
	}

	/// \brief		gets the current time of the stopwatch
	/// \returns	the current time in seconds
	float	getTimeInSec () const
	{
		std::chrono::nanoseconds timeStep = std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::high_resolution_clock::now() - lastTime);
		return (float)(timeStep.count() / 1e9);
	}

	/// \brief		gets the current time of the stopwatch and additionally resets it
	/// \returns	the current time in seconds
	float	getTimeInSecAndReset ()
	{
		float timeStep = getTimeInSec();
		reset();
		return timeStep;
	}

	/// \brief		gets the current time of the stopwatch and additionally resets it
	/// \returns	the current time in milliseconds
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
	/// \brief		default constructor for the StopWatch class
	StopWatch ()
	{ reset(); }

	/// \brief		default destructor for the StopWatch class
	~StopWatch ()
	{}

	//=================================================================================================================
	// basic functions
	//=================================================================================================================

	/// \brief		resets the stopwatch
	void	reset ()
	{
		timeval timePoint;
		gettimeofday(&timePoint, NULL);
		lastTime = (double)(timePoint.tv_sec) * 1e3 + (double)(timePoint.tv_usec) * 1e-3;
	}

	/// \brief		gets the current time of the stopwatch
	/// \returns	the current time in milliseconds
	float	getTimeInMilliSec () const
	{
		timeval timePoint;
		gettimeofday(&timePoint, NULL);
		double newTime = (double)(timePoint.tv_sec) * 1e3 + (double)(timePoint.tv_usec) * 1e-3;
		return newTime - lastTime;
	}

	/// \brief		gets the current time of the stopwatch and additionally resets it
	/// \returns	the current time in milliseconds
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

    writerHeader.close();
  }

}
