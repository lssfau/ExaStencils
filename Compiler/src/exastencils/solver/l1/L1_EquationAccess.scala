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

import exastencils.base.ProgressLocation
import exastencils.datastructures._
import exastencils.knowledge.l1.L1_LeveledKnowledgeAccess
import exastencils.prettyprinting.PpStream
import exastencils.solver.l2.L2_EquationAccess

/// L1_EquationAccess

object L1_EquationAccess {
  def apply(access : L1_FutureEquationAccess) =
    new L1_EquationAccess(L1_EquationCollection.getByIdentifier(access.name, access.level).get)
}

case class L1_EquationAccess(var target : L1_NamedEquation) extends L1_LeveledKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name << '@' << target.level
  override def progress = ProgressLocation(L2_EquationAccess(target.getProgressedObj(), None))
}

/// L1_ResolveEquationAccesses

object L1_ResolveEquationAccesses extends DefaultStrategy("Resolve accesses to equations") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L1_FutureEquationAccess if L1_EquationCollection.exists(access.name, access.level) =>
      access.toEquationAccess
  })
}
