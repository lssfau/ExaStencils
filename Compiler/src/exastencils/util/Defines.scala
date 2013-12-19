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

#define COARSE_GRID_SOLVER_IP_SMOOTHER

//	GS		= Gauss-Seidel
//	GSAC	= Gauss-Seidel with Additional Communication
//	GSOD	= Gauss-Seidel of Death (with additional communication)
//	GSBEAC	= Gauss-Seidel Block Edition with Additional Communication
//	GSRS	= Gauss-Seidel with Random Sampling
//	GSRB	= Gauss-Seidel Red-Black (RBGS)
//	GSRBAC	= Gauss-Seidel Red-Black (RBGS) with Additional Communication

//#define SMOOTHER_JACOBI
#define SMOOTHER_GS
//#define SMOOTHER_GSAC
//#define SMOOTHER_GSOD
//#define SMOOTHER_GSACBE
//#define SMOOTHER_GSRS
//#define SMOOTHER_GSRB
//#define SMOOTHER_GSRBAC

#if defined(SMOOTHER_GSAC) || defined(SMOOTHER_GSOD) || defined(SMOOTHER_GSACBE) || defined(SMOOTHER_GSRBAC)
#	define USE_ADDITIONAL_SMOOTHER_COMM
#endif

#ifdef SMOOTHER_GSOD
#	define NUM_GSOD_ITERATIONS			8
#endif
#ifdef SMOOTHER_GSACBE
#	define NUM_GSBE_ITERATIONS			12
#	define GSBE_WINDOW_SIZE				4
#	define GSBE_WINDOW_OVERLAP			1
#endif

#ifdef SMOOTHER_JACOBI
#	define NUM_SOL_SLOTS				2
#else
#	define NUM_SOL_SLOTS				1
#endif

#endif	// UTIL_DEFINES_H
""");

    writerHeader.close();
  }

}
