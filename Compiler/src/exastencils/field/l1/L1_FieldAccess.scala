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

package exastencils.field.l1

import exastencils.base.ProgressLocation
import exastencils.datastructures._
import exastencils.field.l2.L2_FieldAccess
import exastencils.knowledge.l1._
import exastencils.prettyprinting.PpStream

/// L1_FieldAccess

object L1_FieldAccess {
  def apply(access : L1_FutureFieldAccess) =
    new L1_FieldAccess(L1_FieldCollection.getByIdentifier(access.name, access.level).get)
}

case class L1_FieldAccess(var target : L1_Field) extends L1_LeveledKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name << '@' << target.level
  override def progress = ProgressLocation(L2_FieldAccess(target.getProgressedObj(), None))
}

/// L1_ResolveFieldAccesses

object L1_ResolveFieldAccesses extends DefaultStrategy("Resolve accesses to fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L1_FutureFieldAccess if L1_FieldCollection.exists(access.name, access.level) =>
      access.toFieldAccess
  })
}
