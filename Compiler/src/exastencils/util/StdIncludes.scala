package exastencils.util

import java.io.PrintWriter
import java.io.File

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

import exastencils.primitives.Globals

case class StdIncludes() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def printToFile = {
    val writerHeader = new PrintWriter(new File(Globals.printPath + s"Util/StdIncludes.h"));

    writerHeader.write("""
#ifndef	UTIL_STDINCLUDES_H
#define	UTIL_STDINCLUDES_H

//=====================================================================================================================
//									 _____           ____  _                  _ _     
//									| ____|_  ____ _/ ___|| |_ ___ _ __   ___(_) |___ 
//									|  _| \ \/ / _` \___ \| __/ _ \ '_ \ / __| | / __|
//									| |___ >  < (_| |___) | ||  __/ | | | (__| | \__ \
//									|_____/_/\_\__,_|____/ \__\___|_| |_|\___|_|_|___/
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
/// \file	stdIncludes.h
/// \brief	Summarizes includes used by almost all modules
/// \author	Sebastian Kuckuk
//
//=====================================================================================================================

//=====================================================================================================================
// includes
//=====================================================================================================================

#include "Util/Defines.h"
#include "Util/Log.h"
#include "Util/TypeDefs.h"

#endif	// UTIL_STDINCLUDES_H
""");

    writerHeader.close();
  }

}
