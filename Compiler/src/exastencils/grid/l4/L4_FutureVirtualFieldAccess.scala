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

package exastencils.grid.l4

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l4.L4_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l4.L4_LevelCollector

/// L4_FutureVirtualFieldAccess

case class L4_FutureVirtualFieldAccess(
    var name : String,
    var level : Int,
    var offset : Option[L4_ConstIndex] = None,
    var matIndex : Option[L4_MatIndex] = None) extends L4_FutureKnowledgeAccess with L4_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << '@' << offset.get
    if (matIndex.isDefined) out << matIndex.get
  }

  override def progress = Logger.error(s"Trying to progress future field access to $name on level $level")
  def toVirtualFieldAccess = L4_VirtualFieldAccess(this)
}

/// L4_PrepareVirtualFieldAccesses

object L4_PrepareVirtualFieldAccesses extends DefaultStrategy("Prepare accesses to virtual fields") {
  val collector = new L4_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L4_UnresolvedAccess if L4_VirtualFieldCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to field ${ access.name }")
      }

      if (!L4_VirtualFieldCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      if (access.dirAccess.isDefined) Logger.warn(s"Discarding meaningless direction access on ${ access.name } - was an offset access (@) intended?")
      if (access.slot.isDefined) Logger.warn(s"Discarding meaningless slot access on ${ access.name }")

      L4_FutureVirtualFieldAccess(access.name, lvl, access.offset, access.matIndex)
  })
}
