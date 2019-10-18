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

package exastencils.discretization.l1

import scala.collection.mutable.ListBuffer

import exastencils.base.l1._
import exastencils.prettyprinting.PpStream
import exastencils.solver.l1.L1_EquationCollection

/// L1_EquationDiscretization

case class L1_EquationDiscretization(
    var src : String,
    var levels : Option[L1_DeclarationLevelSpecification],
    var mapping : Option[String]) extends L1_DiscretizationHint {

  override def prettyprint(out : PpStream) = {
    out << src
    if (mapping.isDefined) out << " => " << mapping.get
  }

  override def process() = {
    val equations = {
      if (levels.isDefined) // explicit level definition is given -> extract levels and map to equations
        L1_LevelSpecification.extractLevelListDefEmpty(levels).to[ListBuffer].map(lvl => L1_EquationCollection.getByIdentifier(src, lvl).get)
      else // otherwise collect all equations with the src name
        L1_EquationCollection.getAllByIdentifier(src)
    }

    equations.foreach(l1Equation => {
      val l2Equation = l1Equation.getProgressedObj()

      if (mapping.isDefined) l2Equation.name = mapping.get
    })
  }
}
