//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.boundary.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.communication._
import exastencils.communication.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.domain.ir.IR_IV_NeighborIsValid
import exastencils.field.ir._

/// IR_IsOnBoundary

case class IR_IsOnBoundary(var field : IR_Field, var index : IR_ExpressionIndex) extends IR_Expression with IR_Expandable {
  override def datatype = IR_UnitDatatype

  override def expand() : Output[IR_Expression] = {
    var applicableNeighbors = DefaultNeighbors.neighbors
    if (Knowledge.experimental_bc_checkOnlyMainAxis)
      applicableNeighbors = applicableNeighbors.filter(n => 1 == n.dir.map(d => math.abs(d)).sum)

    if (Knowledge.experimental_bc_avoidOrOperations)
      IR_Negation(applicableNeighbors.map(n => IR_Negation(IR_IsOnSpecBoundary(field, n, index).expand().inner) : IR_Expression).reduce((a, b) => IR_AndAnd(a, b)))
    else
      applicableNeighbors.map(n => IR_IsOnSpecBoundary(field, n, index).expand().inner).reduce((a, b) => IR_OrOr(a, b))
  }
}

/// IR_IsOnSpecBoundary

case class IR_IsOnSpecBoundary(var field : IR_Field, var neigh : NeighborInfo, var index : IR_ExpressionIndex) extends IR_Expression with IR_Expandable {
  override def datatype = IR_UnitDatatype

  override def expand() : Output[IR_Expression] = {
    // should work for node, cell and face localizations

    var conditions = ListBuffer[IR_Expression](IR_Negation(IR_IV_NeighborIsValid(field.domain.index, neigh.index)))
    for (dim <- 0 until field.layout.numDimsGrid) {
      neigh.dir(dim) match {
        case -1 => conditions += IR_Lower(Duplicate(index(dim)), field.layout.idxById("DLE", dim) - field.referenceOffset(dim))
        case 1  => conditions += IR_GreaterEqual(Duplicate(index(dim)), field.layout.idxById("DRB", dim) - field.referenceOffset(dim))
        case 0  => // true
      }
    }

    conditions.reduce((a, b) => IR_AndAnd(a, b))
  }
}

/// IR_IsValidComputationPoint

// checks for IR_IsOnBoundary as well as if outside inner/dup layers on fragment transitions
case class IR_IsValidComputationPoint(var field : IR_Field, var index : IR_ExpressionIndex) extends IR_Expression with IR_Expandable {
  override def datatype = IR_UnitDatatype

  override def expand() : Output[IR_Expression] = {
    var applicableNeighbors = DefaultNeighbors.neighbors
    if (Knowledge.experimental_bc_checkOnlyMainAxis)
      applicableNeighbors = applicableNeighbors.filter(n => 1 == n.dir.map(d => math.abs(d)).sum)

    val isNotOnBoundary =
      if (Knowledge.experimental_bc_avoidOrOperations)
        applicableNeighbors.map(n => IR_Negation(IR_IsOnSpecBoundary(field, n, index).expand().inner) : IR_Expression).reduce((a, b) => IR_AndAnd(a, b))
      else
        IR_Negation(applicableNeighbors.map(n => IR_IsOnSpecBoundary(field, n, index).expand().inner).reduce((a, b) => IR_OrOr(a, b)))

    val isInnerOrDup =
      (0 until field.layout.numDimsGrid).map(dim =>
        IR_AndAnd(
          IR_Lower(Duplicate(index(dim)), field.layout.idxById("DRE", dim) - field.referenceOffset(dim)),
          IR_GreaterEqual(Duplicate(index(dim)), field.layout.idxById("DLB", dim) - field.referenceOffset(dim)))).reduce((a, b) => IR_AndAnd(a, b))

    IR_AndAnd(isNotOnBoundary, isInnerOrDup)
  }
}

/// IR_ResolveBoundaryFunctions

object IR_ResolveBoundaryFunctions extends DefaultStrategy("ResolveBoundaryFunctions") {
  def getIndex(fieldAccess : IR_FieldAccess) = {
    val index = fieldAccess.index
    if (fieldAccess.offset.isDefined)
      for (i <- 0 until Math.min(fieldAccess.index.length, fieldAccess.offset.get.length))
        index(i) += fieldAccess.offset.get(i)
    index
  }

  this += new Transformation("ResolveFunctionCalls", {
    case IR_FunctionCall(IR_UnresolvedFunctionReference("isOnBoundaryOf", _), args) =>
      val fieldAccess = args.head.asInstanceOf[IR_FieldAccess]
      IR_IsOnBoundary(fieldAccess.field, getIndex(fieldAccess))

    case IR_FunctionCall(IR_UnresolvedFunctionReference("isOnEastBoundaryOf", _), args) =>
      val fieldAccess = args.head.asInstanceOf[IR_FieldAccess]
      IR_IsOnSpecBoundary(fieldAccess.field, DefaultNeighbors.getNeigh(Array(1, 0, 0)), getIndex(fieldAccess))

    case IR_FunctionCall(IR_UnresolvedFunctionReference("isOnWestBoundaryOf", _), args) =>
      val fieldAccess = args.head.asInstanceOf[IR_FieldAccess]
      IR_IsOnSpecBoundary(fieldAccess.field, DefaultNeighbors.getNeigh(Array(-1, 0, 0)), getIndex(fieldAccess))

    case IR_FunctionCall(IR_UnresolvedFunctionReference("isOnNorthBoundaryOf", _), args) =>
      val fieldAccess = args.head.asInstanceOf[IR_FieldAccess]
      IR_IsOnSpecBoundary(fieldAccess.field, DefaultNeighbors.getNeigh(Array(0, 1, 0)), getIndex(fieldAccess))

    case IR_FunctionCall(IR_UnresolvedFunctionReference("isOnSouthBoundaryOf", _), args) =>
      val fieldAccess = args.head.asInstanceOf[IR_FieldAccess]
      IR_IsOnSpecBoundary(fieldAccess.field, DefaultNeighbors.getNeigh(Array(0, -1, 0)), getIndex(fieldAccess))

    case IR_FunctionCall(IR_UnresolvedFunctionReference("isOnTopBoundaryOf", _), args) =>
      val fieldAccess = args.head.asInstanceOf[IR_FieldAccess]
      IR_IsOnSpecBoundary(fieldAccess.field, DefaultNeighbors.getNeigh(Array(0, 0, 1)), getIndex(fieldAccess))

    case IR_FunctionCall(IR_UnresolvedFunctionReference("isOnBottomBoundaryOf", _), args) =>
      val fieldAccess = args.head.asInstanceOf[IR_FieldAccess]
      IR_IsOnSpecBoundary(fieldAccess.field, DefaultNeighbors.getNeigh(Array(0, 0, -1)), getIndex(fieldAccess))

    case IR_FunctionCall(IR_UnresolvedFunctionReference("getNeighFragEdge", _), args) =>
      // usage: getNeighFragEdge ( fragmentIdx, neighIdx )
      IR_IV_CommNeighNeighIdx(0, args(1), args(0))

    case IR_FunctionCall(IR_UnresolvedFunctionReference("getNeighFragId", _), args) =>
      // usage: getNeighFragId ( fragmentIdx, neighIdx )
      IR_IV_NeighFragId(0, args(1), args(0))

    case IR_FunctionCall(IR_UnresolvedFunctionReference("getBoundaryConditionId", _), args) =>
      // usage: getBoundaryConditionId ( fragmentIdx, neighIdx )
      IR_IV_BoundaryConditionId(0, args(1), args(0))

    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("setBoundaryConditionId", _), args)) =>
      // usage: setBoundaryConditionId ( fragmentIdx, neighIdx, newId )
      IR_Assignment(IR_IV_BoundaryConditionId(0, args(1), args(0)), args(2))

  })
}


