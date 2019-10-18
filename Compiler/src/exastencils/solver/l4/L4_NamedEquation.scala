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

package exastencils.solver.l4

import exastencils.base.l4._
import exastencils.core.Duplicate
import exastencils.knowledge.l4._
import exastencils.optimization.l4.L4_GeneralSimplifyWrapper
import exastencils.prettyprinting._
import exastencils.solver.ir.IR_NamedEquation

/// L4_NamedEquation

case class L4_NamedEquation(
    var name : String, var level : Int,
    var equation : L4_Equation) extends L4_LeveledKnowledgeObject[IR_NamedEquation] {

  def lhs = equation.lhs
  def rhs = equation.rhs

  override def createDuplicate() = L4_NamedEquation(name, level, Duplicate(equation))

  override def prettyprintDecl(out : PpStream) = {
    out << "Equation " << name << '@' << level << " {\n"
    out << lhs << " == " << rhs
    out << "\n}"
  }

  override def progressImpl() = IR_NamedEquation(name, level, equation.progress)

  def asZeroEquation() : L4_Expression = {
    val zeroEq : L4_Expression = Duplicate(lhs - rhs)
    L4_GeneralSimplifyWrapper.process(zeroEq)
  }
}
