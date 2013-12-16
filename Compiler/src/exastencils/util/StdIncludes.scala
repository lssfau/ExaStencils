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

#include "Util/Defines.h"
#include "Util/Log.h"
#include "Util/TypeDefs.h"

#endif	// UTIL_STDINCLUDES_H
""");

    writerHeader.close();
  }

}
