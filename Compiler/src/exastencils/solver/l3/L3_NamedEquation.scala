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

package exastencils.solver.l3

import exastencils.base.l3._
import exastencils.core.Duplicate
import exastencils.knowledge.l3.L3_LeveledKnowledgeObject
import exastencils.optimization.l3.L3_GeneralSimplifyWrapper
import exastencils.prettyprinting._
import exastencils.solver.l4.L4_NamedEquation

/// L3_NamedEquation

case class L3_NamedEquation(
    var name : String, var level : Int,
    var equation : L3_Equation) extends L3_LeveledKnowledgeObject[L4_NamedEquation] {

  def lhs = equation.lhs
  def rhs = equation.rhs

  override def createDuplicate() = L3_NamedEquation(name, level, Duplicate(equation))

  override def prettyprintDecl(out : PpStream) = {
    out << "Equation " << name << '@' << level << " {\n"
    out << lhs << " == " << rhs
    out << "\n}"
  }

  override def progressImpl() = L4_NamedEquation(name, level, equation.progress)

  def asZeroEquation() : L3_Expression = {
    val zeroEq : L3_Expression = Duplicate(lhs - rhs)
    L3_GeneralSimplifyWrapper.process(zeroEq)
  }
}
