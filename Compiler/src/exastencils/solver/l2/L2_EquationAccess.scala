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

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.datastructures._
import exastencils.knowledge.l2.L2_LeveledKnowledgeAccess
import exastencils.prettyprinting.PpStream
import exastencils.solver.l3.L3_EquationAccess

/// L2_EquationAccess

object L2_EquationAccess {
  def apply(access : L2_FutureEquationAccess) =
    new L2_EquationAccess(L2_EquationCollection.getByIdentifier(access.name, access.level).get, access.offset)
}

case class L2_EquationAccess(
    var target : L2_NamedEquation,
    var offset : Option[L2_ConstIndex] = None) extends L2_LeveledKnowledgeAccess with L2_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    out << target.name << '@' << target.level
    if (offset.isDefined) out << '@' << offset.get
  }

  override def progress = ProgressLocation(L3_EquationAccess(target.getProgressedObj(), L2_ProgressOption(offset)(_.progress)))
}

/// L2_ResolveEquationAccesses

object L2_ResolveEquationAccesses extends DefaultStrategy("Resolve accesses to equations") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L2_FutureEquationAccess if L2_EquationCollection.exists(access.name, access.level) =>
      access.toEquationAccess
  })
}
