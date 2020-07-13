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
import exastencils.core._
import exastencils.prettyprinting._
import exastencils.util.ir._

/// IR_HackMatComponentAccess
// FIXME: update with actual accessors
case class IR_HackMatComponentAccess(var mat : IR_VariableAccess, var i : IR_Expression, var j : IR_Expression) extends IR_Expression {
  override def datatype = mat.datatype

  override def prettyprint(out : PpStream) : Unit = out << mat << "(" << i << ", " << j << ")"
}

/// IR_MatrixExpression
object IR_MatrixExpression {
  var matTmpCounter = 0

  //def apply(innerDatatype : Option[IR_Datatype], rows : Integer, columns : Integer) : IR_MatrixExpression = new IR_MatrixExpression(innerDatatype, rows, columns)
  def apply(innerDatatype : IR_Datatype, rows : Integer, columns : Integer) : IR_MatrixExpression = new IR_MatrixExpression(Some(innerDatatype), rows, columns)

  def apply(innerDatatype : Option[IR_Datatype], rows : Integer, columns : Integer, expressions : Array[IR_Expression]) : IR_MatrixExpression = {
    val tmp = new IR_MatrixExpression(innerDatatype, rows, columns)
    tmp.expressions = expressions
    tmp
  }
  def apply(innerDatatype : Option[IR_Datatype], rows : Integer, columns : Integer, expressions : Array[IR_Expression], shape: Option[IR_MatShape]) : IR_MatrixExpression = {
    val tmp = new IR_MatrixExpression(innerDatatype, rows, columns, shape)
    tmp.expressions = expressions
    tmp
  }


  def apply(innerDatatype : Option[IR_Datatype], expressions : ListBuffer[ListBuffer[IR_Expression]]) : IR_MatrixExpression = {
    val rows = expressions.size
    val columns = expressions(0).size
    val tmp = new IR_MatrixExpression(innerDatatype, rows, columns)
    for (row <- 0 until rows) {
      for (col <- 0 until columns) {
        tmp.set(row, col, expressions(row)(col))
      }
    }
    tmp
  }
  def apply(datatype : IR_MatrixDatatype, expressions : ListBuffer[IR_Expression]) : IR_MatrixExpression = {
    val tmp = IR_MatrixExpression(datatype.datatype, datatype.sizeM, datatype.sizeN)
    tmp.expressions = expressions.toArray
    tmp
  }

  def fromSingleExpression(innerDatatype : IR_Datatype, rows : Integer, columns : Integer, expression : IR_Expression) : IR_MatrixExpression = {
    val tmp = new IR_MatrixExpression(Some(innerDatatype), rows, columns)
    for (i <- 0 until rows * columns)
      tmp.expressions(i) = Duplicate(expression)
    tmp
  }
}

case class IR_MatrixExpression(var innerDatatype : Option[IR_Datatype], var rows : Int, var columns : Int, shape : Option[IR_MatShape] = None) extends IR_Expression {
  var expressions : Array[IR_Expression] = Array.ofDim[IR_Expression](rows * columns)

  override def datatype = {
    innerDatatype match {
      case None                         =>
        var ret = expressions(0).datatype
        expressions.foreach(s => ret = IR_ResultingDatatype(ret, s.datatype))
        innerDatatype = Some(ret)
      case Some(dt : IR_MatrixDatatype) => innerDatatype = Some(dt.resolveBaseDatatype)
      case _                            =>
    }
    IR_MatrixDatatype(innerDatatype.getOrElse(IR_RealDatatype), this.rows, this.columns)
  }

  def prettyprintInner(out : PpStream) : Unit = {
    out << '{' << expressions.map(_.prettyprint).mkString(", ") << '}'
  }

  override def prettyprint(out : PpStream) : Unit = {
    out << "__matrix_"
    innerDatatype.getOrElse(IR_RealDatatype).prettyprint(out)
    out << '_' << rows << "_" << columns << "_t "
    prettyprintInner(out)
  }

  def isConstant = expressions.forall(e => e.isInstanceOf[IR_Number])

  def isInteger = expressions.forall(e => e.isInstanceOf[IR_IntegerConstant])

  def isReal = expressions.forall(e => e.isInstanceOf[IR_RealConstant])

  def get(row : Integer, column : Integer) = expressions(row * columns + column)

  def set(row : Integer, column : Integer, exp : IR_Expression) = expressions(row * columns + column) = exp

  def inverse : IR_MatrixExpression = {
    IR_CompiletimeInversion.inverse(this,  IR_MatShape("filled"))
  }

  override def toString : String = {
    "IR_MatrixExpression(" + innerDatatype + ", " + rows + ", " + columns + "; Items: " + expressions.mkString(", ") + ")"
  }
}
