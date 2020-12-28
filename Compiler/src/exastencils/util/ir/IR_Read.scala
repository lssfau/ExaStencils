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

package exastencils.util.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.prettyprinting.PpStream

trait ReadStream extends IR_Statement {
  def vAccStream : IR_VariableAccess
  def exprToRead : ListBuffer[IR_Expression]
}

/// IR_Read

object IR_Read {
  def apply(stream : IR_VariableAccess, toRead : IR_Expression*) = new IR_Read(stream, toRead.to[ListBuffer])
}

case class IR_Read(var stream : IR_VariableAccess, var toRead : ListBuffer[IR_Expression]) extends ReadStream {
  override def vAccStream : IR_VariableAccess = stream
  override def exprToRead : ListBuffer[IR_Expression] = toRead
  override def prettyprint(out : PpStream) = out << stream << " >> " <<< (toRead, " >> ") << ';'
}

/// IR_ReadBinary

object IR_ReadBinary {
  def apply(stream : IR_VariableAccess, toRead : IR_Access*) = new IR_ReadBinary(stream, toRead.to[ListBuffer])
}

case class IR_ReadBinary(var stream : IR_VariableAccess, var toRead : ListBuffer[IR_Access]) extends ReadStream {
  override def vAccStream : IR_VariableAccess = stream
  override def exprToRead : ListBuffer[IR_Expression] = toRead.asInstanceOf[ListBuffer[IR_Expression]]
  override def prettyprint(out : PpStream) = {
    toRead.foreach(acc => {
      out << IR_MemberFunctionCall(stream, "read",  IR_Cast(IR_PointerDatatype(IR_CharDatatype), IR_AddressOf(acc)), IR_IntegerConstant(acc.datatype.resolveBaseDatatype.typicalByteSize))
      out << ";" << (if (toRead.last equals acc) "" else "\n")
    })
  }
}