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

package exastencils.solver.l1

import exastencils.base.l1._
import exastencils.core.Duplicate
import exastencils.knowledge.l1.L1_LeveledKnowledgeObject
import exastencils.optimization.l1.L1_GeneralSimplifyWrapper
import exastencils.prettyprinting._
import exastencils.solver.l2.L2_NamedEquation

/// L1_NamedEquation

case class L1_NamedEquation(
    var name : String, var level : Int,
    var equation : L1_Equation) extends L1_LeveledKnowledgeObject[L2_NamedEquation] {

  def lhs = equation.lhs
  def rhs = equation.rhs

  override def prettyprintDecl(out : PpStream) = {
    out << "Equation " << name << '@' << level << " {\n"
    out << lhs << " == " << rhs
    out << "\n}"
  }

  override def progressImpl() = L2_NamedEquation(name, level, equation.progress)

  def asZeroEquation() : L1_Expression = {
    val zeroEq : L1_Expression = Duplicate(lhs - rhs)
    L1_GeneralSimplifyWrapper.process(zeroEq)
  }
}
