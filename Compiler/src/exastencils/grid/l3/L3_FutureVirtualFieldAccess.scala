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

package exastencils.grid.l3

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.datastructures._
import exastencils.grid.l4.L4_FutureVirtualFieldAccess
import exastencils.knowledge.l3.L3_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l3.L3_LevelCollector

/// L3_FutureVirtualFieldAccess

case class L3_FutureVirtualFieldAccess(
    var name : String, var level : Int,
    var offset : Option[L3_ConstIndex] = None) extends L3_FutureKnowledgeAccess {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << '@' << offset.get
  }

  override def progress = ProgressLocation {
    Logger.warn(s"Trying to progress future field access to $name on level $level")
    L4_FutureVirtualFieldAccess(name, level, L3_ProgressOption(offset)(_.progress))
  }

  def toVirtualFieldAccess = L3_VirtualFieldAccess(this)
}

/// L3_PrepareVirtualFieldAccesses

object L3_PrepareVirtualFieldAccesses extends DefaultStrategy("Prepare accesses to virtual fields") {
  val collector = new L3_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L3_UnresolvedAccess if L3_VirtualFieldCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to field ${ access.name }")
      }

      if (!L3_VirtualFieldCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      if (access.slot.isDefined) Logger.warn(s"Discarding meaningless slot access on ${ access.name }")
      if (access.dirAccess.isDefined) Logger.warn(s"Discarding meaningless direction access on ${ access.name }")
      if (access.arrayIndex.isDefined) Logger.warn(s"Discarding meaningless array access on ${ access.name }")

      L3_FutureVirtualFieldAccess(access.name, lvl, access.offset)
  })
}
