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

package exastencils.solver.ir

import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.knowledge.ir.IR_LeveledKnowledgeObject
import exastencils.optimization.ir.IR_GeneralSimplifyWrapper

/// IR_NamedEquation

case class IR_NamedEquation(
    var name : String, var level : Int,
    var equation : IR_Equation) extends IR_LeveledKnowledgeObject {

  def lhs = equation.lhs
  def rhs = equation.rhs

  override def createDuplicate() = IR_NamedEquation(name, level, Duplicate(equation))

  def asZeroEquation() : IR_Expression = {
    val zeroEq : IR_Expression = Duplicate(lhs - rhs)
    IR_GeneralSimplifyWrapper.process(zeroEq)
  }
}
