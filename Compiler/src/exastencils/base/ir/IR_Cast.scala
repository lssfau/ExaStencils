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

import exastencils.prettyprinting.PpStream

/// IR_CastLike

trait IR_CastLike extends IR_Expression {
  def toCast : IR_Expression
  def datatype : IR_Datatype
}

/// IR_Cast

case class IR_Cast(var datatype : IR_Datatype, var toCast : IR_Expression) extends IR_CastLike {
  override def prettyprint(out : PpStream) : Unit = out << "((" << datatype << ")" << toCast << ")"
}

/// IR_DynamicCast

case class IR_DynamicCast(var datatype : IR_Datatype, var toCast : IR_Expression) extends IR_CastLike {
  override def prettyprint(out : PpStream) : Unit = out << "dynamic_cast <" << datatype << "> (" << toCast << ")"
}

/// IR_ToInt

case class IR_ToInt(var toCase : IR_Expression) extends IR_Expression with IR_Expandable {
  override def datatype = IR_IntegerDatatype
  override def expand() = IR_Cast(IR_IntegerDatatype, IR_FunctionCall("floor", toCase))
}
