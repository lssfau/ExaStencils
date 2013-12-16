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

//=====================================================================================================================
//									 _____           ____  _                  _ _     
//									| ____|_  ____ _/ ___|| |_ ___ _ __   ___(_) |___ 
//									|  _| \ \/ / _` \___ \| __/ _ \ '_ \ / __| | / __|
//									| |___ >  < (_| |___) | ||  __/ | | | (__| | \__ \
//									|_____/_/\_\__,_|____/ \__\___|_| |_|\___|_|_|___/
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
/// \file	Defines.h
/// \brief	Header file summarizing various defines, enums and helper structs
/// \author	Sebastian Kuckuk
//
//=====================================================================================================================

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

	extern unsigned int	NUM_LSE_ENTRIES_PER_RANK;
	extern unsigned int	COARSE_GRID_RANK_STRIDE;

#	define	NUM_LEVELS					6
#else
#	define	FINAL_LEVEL					8
#	define	COARSE_LEVEL				0
#	define	NUM_LEVELS					(FINAL_LEVEL + 1)

#	define	NUM_COARSE_STEPS			64
#	define	NUM_PRE_SMOOTHING_STEPS		4
#	define	NUM_POST_SMOOTHING_STEPS	2

#	define OMEGA						0.8

#	define NUM_LSE_ENTRIES_PER_RANK		64
#	define COARSE_GRID_RANK_STRIDE		4
#endif

#ifdef SMOOTHER_JACOBI
#	define NUM_SOL_SLOTS				2
#else
#	define NUM_SOL_SLOTS				1
#endif

#if defined(COARSE_GRID_SOLVER_RED_HYPRE) || defined(COARSE_GRID_SOLVER_IP_HYPRE)
#	define COARSE_GRID_SOL_SYNC_ID		(NUM_SOL_SLOTS + 1)
#	define NUM_SYNC_FIELDS				(NUM_SOL_SLOTS + NUM_SOL_SLOTS + 1)
#else
#	define NUM_SYNC_FIELDS				(NUM_SOL_SLOTS + 1)
#endif

#define VERTEX_ID_TO_INDEX_OFFSET		0

#define USE_MPI
#define USE_OMP

//#define OMP_NUM_THREADS					4

//#define ACTIVATE_PADDING

#define NUM_GHOST_LAYERS				1

//#define VERBOSE

//=====================================================================================================================
// enum
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
/// \enum	ELEM_TYPE
/// \brief	specifies possible element types for elements in block-structured domains
//
//=====================================================================================================================
enum ELEM_TYPE
{
	ELEM_INVALID,

	ELEM_TRIANGLE,								///< a 2D triangle with 3 vertices, 3 edges and 0 faces
	ELEM_QUAD,									///< a 2D quadrangle with 4 vertices, 4 edges and 0 faces
	ELEM_TETRAHEDRON,							///< a 3D tetrahedron with 4 vertices, 6 edges and 4 faces
	ELEM_PRISM,									///< a 3D prism with 6 vertices, 9 edges and 5 faces
	ELEM_CUBOID,								///< a 3D cuboid with 8 vertices, 12 edges and 6 faces
};

enum DATA_POSITION
{
	DATA_CELL_CENTERED,
	DATA_NODE_CENTERED,
};

enum VERTEX_LOCATION
{
	VERTEX_INVALID,

	// NOTE: don't change the relative positioning of the following two items (i.e. keep as group and keep ordering)
	VERTEX_EDGE_FROM,
	VERTEX_EDGE_TO,

	// NOTE: don't change the relative positioning of the following four items (i.e. keep as group and keep ordering)
	VERTEX_ELE_QUAD_BL,
	VERTEX_ELE_QUAD_BR,
	VERTEX_ELE_QUAD_TL,
	VERTEX_ELE_QUAD_TR,
};

enum EDGE_LOCATION
{
	EDGE_INVALID,

	// NOTE: don't change the relative positioning of the following four items (i.e. keep as group and keep ordering)
	EDGE_ELE_QUAD_BOTTOM,
	EDGE_ELE_QUAD_RIGHT,
	EDGE_ELE_QUAD_TOP,
	EDGE_ELE_QUAD_LEFT,
};

enum FRAGMENT_LOCATION
{
/*
	// NOTE: don't change the relative positioning of the following six items (i.e. keep as group and keep ordering)
	FRAG_CUBE_BOTTOM,
	FRAG_CUBE_FRONT,
	FRAG_CUBE_RIGHT,
	FRAG_CUBE_BACK,
	FRAG_CUBE_LEFT,
	FRAG_CUBE_TOP,
*/

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

enum DATA_LAYOUT
{
	DATA_2D_QUAD,
	DATA_2D_TRIANGLE,
	DATA_3D_TETRAHEDRON,
	DATA_3D_PRISM,
	DATA_3D_CUBOID,
};

enum BOUNDARY_TYPE
{
	BOUNDARY_NONE,
	BOUNDARY_DIRICHLET,
};

#endif	// UTIL_DEFINES_H
""");

    writerHeader.close();
  }

}
