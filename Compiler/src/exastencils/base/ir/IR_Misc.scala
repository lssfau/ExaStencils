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

package exastencils.base.ir

import scala.collection.mutable.ListBuffer

import exastencils.prettyprinting.PpStream
import exastencils.util.ir._

/// IR_Define

object IR_Define {
  def apply(n : IR_Expression, v : IR_Expression) = new IR_Define(n, Option(v))
}

case class IR_Define(var name : IR_Expression, var value : Option[IR_Expression] = None) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "#define " << name
    if (value.isDefined)
      out << ' ' << value.get
  }
}

/// IR_Comment

case class IR_Comment(var comment : String) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "/* " << comment << " */"
}

/// IR_Assert

case class IR_Assert(var check : IR_Expression, var msg : ListBuffer[IR_Expression], var abort : IR_Statement) extends IR_Statement with IR_Expandable {
  override def expand() = IR_IfCondition(IR_Negation(check), ListBuffer(IR_RawPrint(msg), abort))
}

/// IR_InitializerList

object IR_InitializerList {
  def apply(args : IR_Expression*) = new IR_InitializerList(args.to[ListBuffer])
}

case class IR_InitializerList(var arguments : ListBuffer[IR_Expression]) extends IR_Expression {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "{ " <<< (arguments, ", ") << " }"
}

/// IR_MemberInitializerList

object IR_MemberInitializerList {
  def apply(args : (IR_Access, IR_Expression)*) = new IR_MemberInitializerList(args.to[ListBuffer])
}

case class IR_MemberInitializerList(var arguments : ListBuffer[(IR_Access, IR_Expression)]) extends IR_Expression {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = {
    if (arguments.nonEmpty)
      out << ": "
    for (((member, initVal), i) <- arguments.zipWithIndex)
      out << member << "(" << initVal << ")" << (if (i != arguments.size - 1) ", " else " ")
  }
}