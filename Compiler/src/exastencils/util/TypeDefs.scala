package exastencils.util

import java.io.PrintWriter
import java.io.File

import exastencils.knowledge._

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

case class TypeDefs() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def printToFile = {
    val writerHeader = new PrintWriter(new File(Globals.printPath + s"Util/TypeDefs.h"));

    writerHeader.write("""
#ifndef	UTIL_TYPEDEFS_H
#define	UTIL_TYPEDEFS_H

#endif	// UTIL_TYPEDEFS_H
""");

    writerHeader.close();
  }

}
