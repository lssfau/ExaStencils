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

/// IR_SizeOf

case class IR_SizeOf(var innerDatatype : IR_Datatype) extends IR_Expression {
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << "sizeof" << "(" << innerDatatype << ")"
}

/// IR_ArrayAllocation

case class IR_ArrayAllocation(var name : IR_Expression, // no string - could be an IV
    var innerDatatype : IR_Datatype,
    var size : IR_Expression) extends IR_Statement {
  //override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << name << " = " << "new" << ' ' << innerDatatype << "[" << size << "];"
}

case class IR_ScalarAllocation(var name : IR_Expression, // no string - could be an IV
    var datatype : IR_Datatype,
    ) extends IR_Statement {
  //override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = out << name << " = " << "new" << ' ' << datatype << ";"
}

/// IR_ArrayFree

case class IR_ArrayFree(var pointer : IR_Expression) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "delete[] " << pointer << ";"
}


/// IR_ScalarFree
case class IR_ScalarFree(var pointer : IR_Expression) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "delete " << pointer << ";"
}