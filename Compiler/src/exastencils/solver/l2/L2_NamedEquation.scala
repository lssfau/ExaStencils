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

package exastencils.solver.l2

import exastencils.base.l2._
import exastencils.core.Duplicate
import exastencils.knowledge.l2.L2_LeveledKnowledgeObject
import exastencils.optimization.l2.L2_GeneralSimplifyWrapper
import exastencils.prettyprinting._
import exastencils.solver.l3.L3_NamedEquation

/// L2_NamedEquation

case class L2_NamedEquation(
    var name : String, var level : Int,
    var equation : L2_Equation) extends L2_LeveledKnowledgeObject[L3_NamedEquation] {

  def lhs = equation.lhs
  def rhs = equation.rhs

  override def createDuplicate() = L2_NamedEquation(name, level, Duplicate(equation))

  override def prettyprintDecl(out : PpStream) = {
    out << "Equation " << name << '@' << level << " {\n"
    out << lhs << " == " << rhs
    out << "\n}"
  }

  override def progressImpl() = L3_NamedEquation(name, level, equation.progress)

  def asZeroEquation() : L2_Expression = {
    val zeroEq : L2_Expression = Duplicate(lhs - rhs)
    L2_GeneralSimplifyWrapper.process(zeroEq)
  }
}
