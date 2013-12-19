package exastencils.util

import java.io.PrintWriter
import java.io.File

import exastencils.knowledge._

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

case class Defines() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def printToFile = {
    val writerHeader = new PrintWriter(new File(Globals.printPath + s"Util/Defines.h"));

    writerHeader.write("""
#ifndef	UTIL_DEFINES_H
#define	UTIL_DEFINES_H

#define TIME_MEASUREMENTS

#endif	// UTIL_DEFINES_H
""");

    writerHeader.close();
  }

}
