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

import exastencils.base.ir.IR_Assignment
//import exastencils.base.ir._
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionStatement
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_NullExpression
import exastencils.base.ir.IR_UnresolvedFunctionReference
import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.ir.IR_FieldAccess
import exastencils.prettyprinting.PpStream

/// IR_IV_FragmentOrder

case class IR_IV_FragmentOrder(var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName() = s"fragmentOrder" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(0)
}

/// IR_IV_NeighFragOrder

case class IR_IV_NeighFragOrder(var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, neighIdx)
  override def resolveName() = s"neighFragOrder" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", neighIdx.prettyprint)
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(0)
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
  })
}