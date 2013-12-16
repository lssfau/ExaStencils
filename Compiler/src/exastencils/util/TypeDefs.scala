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

//=====================================================================================================================
//									 _____           ____  _                  _ _     
//									| ____|_  ____ _/ ___|| |_ ___ _ __   ___(_) |___ 
//									|  _| \ \/ / _` \___ \| __/ _ \ '_ \ / __| | / __|
//									| |___ >  < (_| |___) | ||  __/ | | | (__| | \__ \
//									|_____/_/\_\__,_|____/ \__\___|_| |_|\___|_|_|___/
//
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
/// \file	TypeDefs.h
/// \brief	Header file summarizing various type definitions; main point for exchanging primitive and data types
/// \author	Sebastian Kuckuk
//
//=====================================================================================================================

//=====================================================================================================================
// includes
//=====================================================================================================================

#include <vector>

#include <boost/shared_ptr.hpp>

#include "Util/Defines.h"

//=====================================================================================================================
// forward declarations
//=====================================================================================================================

template<unsigned int numLevels>
class Vertex2D;

template<unsigned int numLevels>
class Edge2D;

template<unsigned int numLevels, DATA_POSITION dataPos>
class Element2DQuad;

class Fragment3DCube;

template<class T, DATA_LAYOUT dataLayout>
class Container;

template <typename T> class TVec4;
template <typename T> class TVec3;
template <typename T> class TVec2;

//=====================================================================================================================
// general typedefs
//=====================================================================================================================

typedef	int														MPIRankType;

typedef size_t													exa_id_t;
typedef double													exa_real_t;
typedef	Container<exa_real_t, DATA_3D_CUBOID>					PayloadContainer_1Real;
typedef	Container<TVec2<exa_real_t>, DATA_3D_CUBOID>			PayloadContainer_2Real;
typedef	Container<TVec3<exa_real_t>, DATA_3D_CUBOID>			PayloadContainer_3Real;
typedef	Container<TVec4<exa_real_t>, DATA_3D_CUBOID>			PayloadContainer_4Real;
typedef std::vector</*boost::shared_ptr<*/PayloadContainer_1Real*>/* >*/	ContainerList_1Real;
typedef std::vector</*boost::shared_ptr<*/PayloadContainer_2Real*>/* >*/	ContainerList_2Real;
typedef std::vector</*boost::shared_ptr<*/PayloadContainer_3Real*>/* >*/	ContainerList_3Real;
typedef std::vector</*boost::shared_ptr<*/PayloadContainer_4Real*>/* >*/	ContainerList_4Real;

//=====================================================================================================================
// primitive typedefs
//=====================================================================================================================

typedef Element2DQuad<NUM_LEVELS, DATA_NODE_CENTERED>			CurElem2DQuadType;
typedef Edge2D<NUM_LEVELS>										CurEdge2DType;
typedef Vertex2D<NUM_LEVELS>									CurVertex2DType;

typedef CurElem2DQuadType										CurElemType;
typedef CurEdge2DType											CurEdgeType;
typedef CurVertex2DType											CurVertexType;

typedef Fragment3DCube											CurFragmentType;

#define CUR_VERTEX_TYPE_HEADER									"Primitives/Vertex2D.h"
#define CUR_EDGE_TYPE_HEADER									"Primitives/Edge2D.h"
#define CUR_ELEMENT_TYPE_HEADER									"Primitives/Element2DQuad.h"

#endif	// UTIL_TYPEDEFS_H
        """);

    writerHeader.close();
  }

}
