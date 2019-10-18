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
import exastencils.operator.l3.L3_StencilAccess
import exastencils.prettyprinting.PpStream

/// L2_StencilAccess

object L2_StencilAccess {
  def apply(access : L2_FutureStencilAccess) =
    new L2_StencilAccess(L2_StencilCollection.getByIdentifier(access.name, access.level).get, access.offset, access.dirAccess)
}

case class L2_StencilAccess(
    var target : L2_Stencil,
    var offset : Option[L2_ConstIndex] = None,
    var dirAccess : Option[L2_ConstIndex] = None) extends L2_OperatorAccess with L2_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << '@' << offset.get
    if (dirAccess.isDefined) out << ':' << dirAccess.get
  }

  override def progress = ProgressLocation {
    L3_StencilAccess(target.getProgressedObj(),
      L2_ProgressOption(offset)(_.progress),
      L2_ProgressOption(dirAccess)(_.progress))
  }
  override def assembleOffsetMap() = target.assembleOffsetMap()
}

/// L2_ResolveStencilAccesses

object L2_ResolveStencilAccesses extends DefaultStrategy("Resolve accesses to stencils") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L2_FutureStencilAccess if L2_StencilCollection.exists(access.name, access.level) =>
      access.toStencilAccess
  })
}
