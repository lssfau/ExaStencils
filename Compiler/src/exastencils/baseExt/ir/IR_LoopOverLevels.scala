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

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.Output

object IR_LoopOverLevels {
  def apply(body : IR_Statement*) = new IR_LoopOverLevels(body.to[ListBuffer])

  def defIt = "levelIdx"
}

case class IR_LoopOverLevels(var body : ListBuffer[IR_Statement]) extends IR_ScopedStatement with IR_Expandable {

  import IR_LoopOverLevels._

  override def expand() : Output[IR_ForLoop] = {
    IR_ForLoop(
      IR_VariableDeclaration(IR_IntegerDatatype, defIt, Knowledge.minLevel),
      IR_Lower(defIt, Knowledge.maxLevel + 1),
      IR_PreIncrement(defIt),
      body)
  }
}
