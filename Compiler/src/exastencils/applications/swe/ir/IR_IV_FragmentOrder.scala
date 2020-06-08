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

package exastencils.applications.swe.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.DefaultNeighbors
import exastencils.communication.ir.IR_IV_CommNeighNeighIdx
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.domain.ir._
import exastencils.field.ir.IR_FieldAccess
import exastencils.prettyprinting.PpStream

/// IR_IV_FragmentOrder

case class IR_IV_FragmentOrder(var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName() = s"fragmentOrder" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(-1)
}

/// IR_IV_NeighFragOrder

case class IR_IV_NeighFragOrder(var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, true) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, neighIdx)
  override def resolveName() = s"neighFragOrder" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", neighIdx.prettyprint)
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(-1)
}

/// IR_ResolveFragmentOrder

object IR_ResolveFragmentOrder extends DefaultStrategy("ResolveFragmentOrder") {
  def getIndex(fieldAccess : IR_FieldAccess) = {
    val index = fieldAccess.index
    if (fieldAccess.offset.isDefined)
      for (i <- 0 until Math.min(fieldAccess.index.length, fieldAccess.offset.get.length))
        index(i) += fieldAccess.offset.get(i)
    index
  }

  this += new Transformation("ResolveFunctionCalls", {

    case IR_FunctionCall(IR_UnresolvedFunctionReference("getFragmentOrder", _), args) =>
      // usage: getFragmentOrder ( fragmentIdx )
      IR_IV_FragmentOrder(args(0))

    case IR_FunctionCall(IR_UnresolvedFunctionReference("getNeighFragmentOrder", _), args) =>
      // usage: getNeighFragmentOrder ( fragmentIdx, neighIdx )
      IR_IV_NeighFragOrder(args(1), args(0))

    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("setFragmentOrder", _), args)) =>
      // usage: setFragmentOrder ( fragmentIdx, order )
      IR_Assignment(IR_IV_FragmentOrder(args(0)), args(1))

    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("setNeighFragmentOrder", _), args)) =>
      // usage: setNeighFragmentOrder ( fragmentIdx, neighIdx, order )
      IR_Assignment(IR_IV_NeighFragOrder(args(1), args(0)), args(2))

    case IR_FunctionCall(IR_UnresolvedFunctionReference("getNeighIsValid", _), args) =>
      // usage: getNeighIsValid (  neighIdx )
      IR_IV_NeighborIsValid(0, args(0))

    case IR_ExpressionStatement(call : IR_FunctionCall) if "communicateFragOrder" == call.name =>
      def fragmentIdx = IR_LoopOverFragments.defIt

      var statements = ListBuffer[IR_Statement]()
      for (neigh <- DefaultNeighbors.neighbors) {
        statements += IR_IfCondition(
          IR_IV_NeighborIsValid(0, neigh.index),
          IR_IfCondition(
            IR_IV_NeighborIsRemote(0, neigh.index), IR_Assignment(
              IR_IV_NeighFragOrder(if (Knowledge.comm_enableCommTransformations)
                IR_IV_CommNeighNeighIdx(0, neigh.index)
              else
                DefaultNeighbors.getOpposingNeigh(neigh.index).index, IR_IV_NeighborFragmentIdx(0, neigh.index)),
              IR_IV_FragmentOrder()), //TODO MPI part
            IR_Assignment(
              IR_IV_NeighFragOrder(if (Knowledge.comm_enableCommTransformations)
                IR_IV_CommNeighNeighIdx(0, neigh.index)
              else
                DefaultNeighbors.getOpposingNeigh(neigh.index).index, IR_IV_NeighborFragmentIdx(0, neigh.index)),
              IR_IV_FragmentOrder())))
      }

      IR_LoopOverFragments(statements)
  })
}
