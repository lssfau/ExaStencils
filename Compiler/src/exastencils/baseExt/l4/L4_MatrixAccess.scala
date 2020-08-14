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

package exastencils.baseExt.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir.IR_Expression
import exastencils.base.l4._
import exastencils.baseExt.ir.IR_MatrixAccess
import exastencils.baseExt.ir.IR_MatrixExpression
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_MatrixAccess
object L4_MatrixAccess {
  def apply(name : String, idxy : L4_Index, idxx : L4_Index) : L4_MatrixAccess = {
    idxx match {
      case expridx : L4_ExpressionIndex =>
        if (expridx.indices.length > 1) Logger.error(s"Matrix access with more than 1 indices not allowed")
      case cidx : L4_ConstIndex =>
        if (cidx.indices.length > 1) Logger.error(s"Matrix access with more than 1 indices not allowed")
      case ridx : L4_RangeIndex =>
      case _ => Logger.error("unexprected index type")
    }
    idxy match {
      case expridx : L4_ExpressionIndex =>
        if (expridx.indices.length > 1) Logger.error(s"Matrix access with more than 1 indices not allowed")
      case cidx : L4_ConstIndex =>
        if (cidx.indices.length > 1) Logger.error(s"Matrix access with more than 1 indices not allowed")
      case ridx : L4_RangeIndex =>
      case _ => Logger.error("unexprected index type")
    }
    new L4_MatrixAccess(name, idxy, idxx)
  }
}

case class L4_MatrixAccess(name : String, idxy : L4_Index, idxx : L4_Index) extends L4_Access {
  override def progress : IR_Expression = ProgressLocation(IR_MatrixAccess(name, idxx.progress, idxy.progress, None))
  override def prettyprint(out : PpStream) : Unit = {
    out << name << idxy << idxx
  }
}

/// L4_MatrixExpression

case class L4_MatrixExpression(
    var datatype : Option[L4_Datatype],
    var expressions : List[List[L4_Expression]],
    var shape : Option[L4_MatShape]
) extends L4_Expression {

  if (expressions.exists(_.length != expressions(0).length))
    Logger.error("Rows of matrix must be of equal length")

  def prettyprint(out : PpStream) = {
    out << "{ "
    expressions.foreach(out << "{ " <<< (_, ", ") << " }, ")
    out.removeLast(", ".length)
    out << " }"
    if (shape.isDefined) out << shape.get.toString()
  }

  override def progress = ProgressLocation(
    IR_MatrixExpression(
      L4_ProgressOption(datatype)(_.progress),
      this.rows,
      this.columns,
      expressions.flatten.map(_.progress).toArray,
      if (shape.isDefined) Some(shape.get.progress) else None
    )
  )

  def rows = expressions.length
  def columns = expressions(0).length
  def isConstant = expressions.flatten.count(_.isInstanceOf[L4_Number]) == expressions.length
  def convertConstants(dt : L4_Datatype) : Unit = {
    expressions = expressions.map(_.map(exp => (exp, dt) match {
      case (c : L4_IntegerConstant, L4_RealDatatype | L4_FloatDatatype | L4_DoubleDatatype) => L4_RealConstant(c.v)
      case (c : L4_RealConstant, L4_IntegerDatatype)                                        => L4_IntegerConstant(c.v.toInt)
      case (_, _)                                                                           => exp
    }))
  }
}
