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

import scala.collection.mutable._

import exastencils.base.ExaRootNode
import exastencils.base.ProgressLocation
import exastencils.prettyprinting.PpStream
import exastencils.solver.l3.L3_SolverForEquation

/// L2_SolverForEquation

object L2_SolverForEquation {
  def apply(entries : List[L2_SolverForEqEntry]) = new L2_SolverForEquation(entries.to[ListBuffer])
}

case class L2_SolverForEquation(var entries : ListBuffer[L2_SolverForEqEntry]) extends L2_SolverHint {
  override def prettyprint(out : PpStream) = out << "generate solver for " <<< (entries, " and ")
  override def process() = ExaRootNode.l2_root.nodes += this // append to l2 root; use automatic progression
  override def progress = ProgressLocation(L3_SolverForEquation(entries.map(_.progress)))
}
