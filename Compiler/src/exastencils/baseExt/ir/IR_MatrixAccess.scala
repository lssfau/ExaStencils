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

package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_MatNodes.IR_GetElement
import exastencils.baseExt.ir.IR_MatNodes.IR_GetSlice
import exastencils.baseExt.ir.IR_MatNodes.IR_SetElement
import exastencils.baseExt.ir.IR_MatNodes.IR_SetSlice
import exastencils.datastructures.Transformation.OutputType
import exastencils.logger.Logger
import exastencils.prettyprinting._

/// IR_HackMatComponentAccess
// FIXME: update with actual accessors
case class IR_HackMatComponentAccess(var mat : IR_VariableAccess, var i : IR_Expression, var j : IR_Expression) extends IR_Expression {
  override def datatype = mat.datatype
  override def prettyprint(out : PpStream) : Unit = out << mat << "(" << i << ", " << j << ")"
}

case class IR_MatrixAccess(name : String, idxy : IR_Index, idxx : IR_Index, dt : Option[IR_MatrixDatatype]) extends IR_Access {
  override def datatype : IR_Datatype = dt.getOrElse(Logger.error("Accessing datatype that has not been assigned"))
  override def prettyprint(out : PpStream) : Unit = Logger.error("internal node not resolved!")
  def expand(lval : Boolean, rhsExpr : Option[IR_Expression]) : OutputType = {
    val indices_y : Array[IR_Expression] = idxy match {
      case _ @ IR_ExpressionIndex(idxs) => idxs
      case _ @ IR_ConstIndex(idxs)      => idxs.map(i => IR_IntegerConstant(i))
      case _ @ IR_RangeIndex(range)     => Array[IR_Expression](range(0).begin.getOrElse(IR_IntegerConstant(0)), range(0).end.getOrElse(IR_IntegerConstant(dt.asInstanceOf[IR_MatrixDatatype].sizeM)))
    }
    val indices_x : Array[IR_Expression] = idxx match {
      case _ @ IR_ExpressionIndex(idxs) => idxs
      case _ @ IR_ConstIndex(idxs)      => idxs.map(i => IR_IntegerConstant(i))
      case _ @ IR_RangeIndex(range)     => Array[IR_Expression](range(0).begin.getOrElse(IR_IntegerConstant(0)), range(0).end.getOrElse(IR_IntegerConstant(dt.asInstanceOf[IR_MatrixDatatype].sizeN)))
    }
    (indices_y.length, indices_x.length) match {
      case (1, 1) =>
        if (!lval) IR_GetElement(ListBuffer[IR_Expression](IR_VariableAccess(name, datatype), indices_y(0), indices_x(0)))
        else IR_SetElement(ListBuffer[IR_Expression](IR_VariableAccess(name, datatype), indices_y(0), indices_x(0), rhsExpr.getOrElse(Logger.error("rhs value for MatrixAccess assignment not given"))))
      case (2, 2) =>
        if (!lval) IR_GetSlice(ListBuffer[IR_Expression](IR_VariableAccess(name, datatype), indices_y(0), indices_x(0), indices_y(1) - indices_y(0), indices_x(1) - indices_x(0)))
        else IR_SetSlice(ListBuffer[IR_Expression](IR_VariableAccess(name, datatype), indices_y(0), indices_x(0), indices_y(1) - indices_y(0), indices_x(1) - indices_x(0), rhsExpr.getOrElse(Logger.error("rhs value for MatrixAccess assignment not given"))))
      case _      => Logger.error(s"unexpected index combination: ${ indices_x.length }, ${ indices_y.length }")
    }
  }

}
