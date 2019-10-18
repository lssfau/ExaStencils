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

package exastencils.parallelization.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Statement
import exastencils.prettyprinting.PpStream

/// IR_PotentiallyCritical

object IR_PotentiallyCritical {
  def apply(body : IR_Statement*) = new IR_PotentiallyCritical(body.to[ListBuffer])
  def apply(body : List[IR_Statement]) = new IR_PotentiallyCritical(body.to[ListBuffer])
}

case class IR_PotentiallyCritical(var body : ListBuffer[IR_Statement]) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "{\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}
