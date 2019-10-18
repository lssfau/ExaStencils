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

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.knowledge.l3.L3_LeveledKnowledgeAccess
import exastencils.prettyprinting.PpStream
import exastencils.solver.l4.L4_EquationAccess

/// L3_EquationAccess

object L3_EquationAccess {
  def apply(access : L3_FutureEquationAccess) =
    new L3_EquationAccess(L3_EquationCollection.getByIdentifier(access.name, access.level).get, access.offset)
}

case class L3_EquationAccess(
    var target : L3_NamedEquation,
    var offset : Option[L3_ConstIndex] = None) extends L3_LeveledKnowledgeAccess with L3_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    out << target.name << '@' << target.level
    if (offset.isDefined) out << '@' << offset.get
  }

  override def progress = ProgressLocation(L4_EquationAccess(target.getProgressedObj(), L3_ProgressOption(offset)(_.progress)))
}

/// L3_ResolveEquationAccesses

object L3_ResolveEquationAccesses extends DefaultStrategy("Resolve accesses to equations") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L3_FutureEquationAccess if L3_EquationCollection.exists(access.name, access.level) =>
      access.toEquationAccess
  })
}
