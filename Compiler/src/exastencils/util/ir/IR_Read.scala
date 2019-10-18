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

/// IR_Read

object IR_Read {
  def apply(stream : IR_VariableAccess, toRead : IR_Expression*) = new IR_Read(stream, toRead.to[ListBuffer])
}

case class IR_Read(var stream : IR_VariableAccess, var toRead : ListBuffer[IR_Expression]) extends IR_Statement {
  override def prettyprint(out : PpStream) = out << stream << " >> " <<< (toRead, " >> ") << ';'
}
