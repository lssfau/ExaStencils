package exastencils.util

import java.io.PrintWriter
import java.io.File

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

import exastencils.primitives.Globals

case class Log() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def printToFile = {
    val writerHeader = new PrintWriter(new File(Globals.printPath + s"Util/Log.h"));

    writerHeader.write("""
#ifndef	UTIL_LOG_H
#define	UTIL_LOG_H

//=====================================================================================================================
//									 _____           ____  _                  _ _     
//									| ____|_  ____ _/ ___|| |_ ___ _ __   ___(_) |___ 
//									|  _| \ \/ / _` \___ \| __/ _ \ '_ \ / __| | / __|
//									| |___ >  < (_| |___) | ||  __/ | | | (__| | \__ \
//									|_____/_/\_\__,_|____/ \__\___|_| |_|\___|_|_|___/
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
/// \file	Log.h
/// \brief	Provides various defines for error handling and logging
/// \author	Sebastian Kuckuk
//
//=====================================================================================================================

//=====================================================================================================================
// includes
//=====================================================================================================================

#include <iostream>

//=====================================================================================================================
// output functions
//=====================================================================================================================

/// prints the given info message to the log file
#define LOG_NOTE(msg) \
{ \
	std::cout << "[NOTE] " << __FILE__ << ", " << __LINE__ << ": " << msg << std::endl; \
}

/// prints the given warning to the log file
#define LOG_WARNING(msg) \
{ \
	std::cerr << "[WARN] " << __FILE__ << ", " << __LINE__ << ": " << msg << std::endl; \
}

/// prints the given error to the log file
#define LOG_ERROR(msg) \
{ \
	std::cerr << "[ERR ] " << __FILE__ << ", " << __LINE__ << ": " << msg << std::endl; \
}

//=====================================================================================================================
// asserts
//=====================================================================================================================

/// checks the condition and in case of false, prints the given msg and performs the fail_op
#define	ASSERT_WARNING(condition, msg, fail_op) \
{ \
	if (!(condition)) \
	{ \
		LOG_WARNING(msg); \
		fail_op; \
	} \
}

/// checks the condition and in case of false, prints the given msg and performs the fail_op
#define	ASSERT_ERROR(condition, msg, fail_op) \
{ \
	if (!(condition)) \
	{ \
		LOG_ERROR(msg); \
		fail_op; \
	} \
}

//=====================================================================================================================
// freeing functions
//=====================================================================================================================

/// safely deletes the given pointer; checks for null and gives a warning if necessary; sets pointer to null after freeing
#define SAFE_DELETE(p) \
{ \
	if (0 == p) \
	{ LOG_WARNING("Trying to delete NULL pointer");} \
	else \
	{ \
	delete p; \
	p = NULL; \
	} \
}

/// safely deletes the given pointer; checks for null; sets pointer to null after freeing
#define SAFE_DELETE_SILENT(p) \
{ \
	if (0 != p) \
	{ \
	delete p; \
	p = NULL; \
	} \
}

/// safely deletes the given array pointer; checks for null and gives a warning if necessary; sets pointer to null after freeing
#define SAFE_DELETE_A(p) \
{ \
	if (0 == p) \
	{ LOG_WARNING("Trying to delete NULL pointer");} \
	else \
	{ \
	delete[] p; \
	p = NULL; \
	} \
}

/// safely deletes the given pointer; checks for null; sets pointer to null after freeing
#define SAFE_DELETE_A_SILENT(p) \
{ \
	if (0 != p) \
	{ \
	delete[] p; \
	p = NULL; \
	} \
}

#endif	// UTIL_LOG_H
""");

    writerHeader.close();
  }

}
