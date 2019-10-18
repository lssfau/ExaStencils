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
import exastencils.config.Platform
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_ResultingDatatype

/// IR_HackVecComponentAccess

// FIXME: update with actual accessors
@deprecated("Drop vector types and switch to matrix types")
case class IR_HackVecComponentAccess(var vec : IR_VariableAccess, var i : IR_Expression) extends IR_Expression {
  override def datatype = vec.datatype
  override def prettyprint(out : PpStream) : Unit = out << vec << "(" << i << ", " << 0 << ")"
}

/// IR_VectorExpression

// FIXME: to be replaced/ updated
@deprecated("switch to IR_MatrixExpression")
case class IR_VectorExpression(var innerDatatype : Option[IR_Datatype], var expressions : ListBuffer[IR_Expression], var rowVector : Option[Boolean]) extends IR_Expression {
  def length = expressions.length

  def apply(i : Integer) = expressions(i)
  def isConstant = expressions.forall(e => e.isInstanceOf[IR_Number])

  override def datatype = {
    if (innerDatatype.isEmpty) {
      var ret = expressions(0).datatype
      expressions.foreach(s => ret = IR_ResultingDatatype(ret, s.datatype))
      innerDatatype = Some(ret)
    }
    IR_VectorDatatype(innerDatatype.getOrElse(IR_RealDatatype), expressions.length, rowVector)
  }
  def prettyprintInner(out : PpStream) : Unit = {
    out << (if (Platform.targetCompiler == "GCC") "std::move((" else "((")
    innerDatatype.getOrElse(IR_RealDatatype).prettyprint(out)
    out << "[]){" << (expressions.map(_.prettyprint).mkString(",")) << "})"
  }
  override def prettyprint(out : PpStream) : Unit = {
    out << "Matrix<"
    innerDatatype.getOrElse(IR_RealDatatype).prettyprint(out)
    out << ", "
    if (rowVector.getOrElse(true)) {
      out << "1, " << length << "> (" // row vector
    } else {
      out << length << ", 1> ("
    }
    prettyprintInner(out)
    out << ')'
  }
}
