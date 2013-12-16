package exastencils.util

import java.io.PrintWriter
import java.io.File

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

import exastencils.primitives.Globals

case class Defines() extends Node with FilePrettyPrintable {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def printToFile = {
    val writerHeader = new PrintWriter(new File(Globals.printPath + s"Util/Defines.h"));

    writerHeader.write("""
#ifndef	UTIL_DEFINES_H
#define	UTIL_DEFINES_H

#define TIME_MEASUREMENTS
//#define MEASURE_MG_COMPONENTS
//#define MEASURE_LSE_SETUP

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
#	define NUM_GSOD_ITERATIONS			NUM_COARSE_STEPS
#endif
#ifdef SMOOTHER_GSACBE
#	define NUM_GSBE_ITERATIONS			12
#	define GSBE_WINDOW_SIZE				4
#	define GSBE_WINDOW_OVERLAP			1
#endif

#ifdef TIME_MEASUREMENTS
	extern unsigned int	FINAL_LEVEL;
	extern unsigned int	COARSE_LEVEL;
	extern unsigned int	NUM_COARSE_STEPS;
	extern unsigned int	NUM_PRE_SMOOTHING_STEPS;
	extern unsigned int	NUM_POST_SMOOTHING_STEPS;

	extern double		OMEGA;

#	define	NUM_LEVELS					6
#else
#	define	FINAL_LEVEL					8
#	define	COARSE_LEVEL				0
#	define	NUM_LEVELS					(FINAL_LEVEL + 1)

#	define	NUM_COARSE_STEPS			64
#	define	NUM_PRE_SMOOTHING_STEPS		4
#	define	NUM_POST_SMOOTHING_STEPS	2

#	define OMEGA						0.8
#endif

#ifdef SMOOTHER_JACOBI
#	define NUM_SOL_SLOTS				2
#else
#	define NUM_SOL_SLOTS				1
#endif

#define NUM_SYNC_FIELDS					(NUM_SOL_SLOTS + 1)

#define USE_MPI
#define USE_OMP

//#define OMP_NUM_THREADS					4

//#define ACTIVATE_PADDING

#define NUM_GHOST_LAYERS				1

//#define VERBOSE

enum FRAGMENT_LOCATION
{
	// NOTE: don't change the relative positioning of the following 27 items (i.e. keep as group and keep ordering)
	FRAG_CUBE_ZN_YN_XN,
	FRAG_CUBE_ZN_YN_X0,
	FRAG_CUBE_ZN_YN_XP,
	FRAG_CUBE_ZN_Y0_XN,
	FRAG_CUBE_ZN_Y0_X0,
	FRAG_CUBE_ZN_Y0_XP,
	FRAG_CUBE_ZN_YP_XN,
	FRAG_CUBE_ZN_YP_X0,
	FRAG_CUBE_ZN_YP_XP,

	FRAG_CUBE_Z0_YN_XN,
	FRAG_CUBE_Z0_YN_X0,
	FRAG_CUBE_Z0_YN_XP,
	FRAG_CUBE_Z0_Y0_XN,
	FRAG_CUBE_Z0_Y0_X0,
	FRAG_CUBE_Z0_Y0_XP,
	FRAG_CUBE_Z0_YP_XN,
	FRAG_CUBE_Z0_YP_X0,
	FRAG_CUBE_Z0_YP_XP,

	FRAG_CUBE_ZP_YN_XN,
	FRAG_CUBE_ZP_YN_X0,
	FRAG_CUBE_ZP_YN_XP,
	FRAG_CUBE_ZP_Y0_XN,
	FRAG_CUBE_ZP_Y0_X0,
	FRAG_CUBE_ZP_Y0_XP,
	FRAG_CUBE_ZP_YP_XN,
	FRAG_CUBE_ZP_YP_X0,
	FRAG_CUBE_ZP_YP_XP,

	FRAG_INVALID,
};

#endif	// UTIL_DEFINES_H
""");

    writerHeader.close();
  }

}
