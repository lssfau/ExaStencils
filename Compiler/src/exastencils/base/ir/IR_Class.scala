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

/// IR_ObjectInstantiation

object IR_ObjectInstantiation {
  def apply(datatype : IR_Datatype, name : String, ctorArgs : IR_Expression*) =
    new IR_ObjectInstantiation(datatype, name, ctorArgs.to[ListBuffer])
  def apply(variable : IR_VariableAccess, ctorArgs : IR_Expression*) =
    new IR_ObjectInstantiation(variable.datatype, variable.name, ctorArgs.to[ListBuffer])
}

case class IR_ObjectInstantiation(var datatype : IR_Datatype, var name : String, var ctorArgs : ListBuffer[IR_Expression]) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << datatype.resolveDeclType << ' ' << name << datatype.resolveDeclPostscript
    if (ctorArgs.nonEmpty)
      out << '(' <<< (ctorArgs, ", ") << ')'
    out << ';'
  }
}

/// IR_MemberAccess

case class IR_MemberAccess(var base : IR_Access, var member : String) extends IR_Access {
  // FIXME: datatype
  override def datatype = base.datatype
  override def prettyprint(out : PpStream) : Unit = out << base << '.' << member
}

/// IR_MemberFunctionCall

object IR_MemberFunctionCall {
  def apply(objectName : IR_Expression, name : String, args : IR_Expression*) =
    new IR_MemberFunctionCall(objectName, name, args.to[ListBuffer])
}

case class IR_MemberFunctionCall(var objectName : IR_Expression, var name : String, var arguments : ListBuffer[IR_Expression]) extends IR_Expression {

  // FIXME: datatype
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << objectName << '.' << name << '(' <<< (arguments, ", ") << ')'
}

object IR_MemberFunctionCallArrow {
  def apply(objectName : IR_Expression, name : String, dt : IR_Datatype, args : IR_Expression*) =
    new IR_MemberFunctionCallArrow(objectName, name, args.to[ListBuffer], dt)
}

case class IR_MemberFunctionCallArrow(
    var objectName : IR_Expression,
    var name : String,
    var arguments : ListBuffer[IR_Expression],
    var datatype : IR_Datatype = IR_UnitDatatype
) extends IR_Expression {

  override def prettyprint(out : PpStream) : Unit = out << objectName << "->" << name << '(' <<< (arguments, ", ") << ')'
}
