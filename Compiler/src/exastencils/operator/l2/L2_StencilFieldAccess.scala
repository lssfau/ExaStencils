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

package exastencils.operator.l2

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.datastructures._
import exastencils.knowledge.l2._
import exastencils.operator.l3.L3_StencilFieldAccess
import exastencils.prettyprinting.PpStream

/// L2_StencilFieldAccess

object L2_StencilFieldAccess {
  def apply(access : L2_FutureStencilFieldAccess) =
    new L2_StencilFieldAccess(L2_StencilFieldCollection.getByIdentifier(access.name, access.level).get, access.offset, access.dirAccess)
}

case class L2_StencilFieldAccess(
    var target : L2_StencilField,
    var offset : Option[L2_ConstIndex] = None,
    var dirAccess : Option[L2_ConstIndex] = None) extends L2_LeveledKnowledgeAccess with L2_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << '@' << offset.get
    if (dirAccess.isDefined) out << ':' << dirAccess.get
  }

  override def progress = ProgressLocation {
    L3_StencilFieldAccess(target.getProgressedObj(),
      L2_ProgressOption(offset)(_.progress),
      L2_ProgressOption(dirAccess)(_.progress))
  }
}

/// L2_ResolveStencilFieldAccesses

object L2_ResolveStencilFieldAccesses extends DefaultStrategy("Resolve accesses to stencil fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L2_FutureStencilFieldAccess if L2_StencilFieldCollection.exists(access.name, access.level) =>
      access.toStencilFieldAccess
  })
}
