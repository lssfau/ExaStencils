package exastencils.util

import java.io.PrintWriter
import java.io.File

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

import exastencils.primitives.Globals

case class TypeDefs() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def printToFile = {
    val writerHeader = new PrintWriter(new File(Globals.printPath + s"Util/TypeDefs.h"));

    writerHeader.write("""
#ifndef	UTIL_TYPEDEFS_H
#define	UTIL_TYPEDEFS_H

#include <vector>

#include "Util/Defines.h"

class Fragment3DCube;
class Container;

template <typename T> class TVec4;
template <typename T> class TVec3;
template <typename T> class TVec2;

typedef size_t			exa_id_t;
typedef double			exa_real_t;

#endif	// UTIL_TYPEDEFS_H
""");

    writerHeader.close();
  }

}
