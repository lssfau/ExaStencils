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

package exastencils.operator.l3

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.datastructures._
import exastencils.field.l4.L4_ActiveSlot
import exastencils.knowledge.l3.L3_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.operator.l4.L4_FutureStencilFieldAccess
import exastencils.prettyprinting.PpStream
import exastencils.util.l3.L3_LevelCollector

/// L3_FutureStencilFieldAccess

case class L3_FutureStencilFieldAccess(
    var name : String, var level : Int,
    var offset : Option[L3_ConstIndex],
    var dirAccess : Option[L3_ConstIndex]) extends L3_FutureKnowledgeAccess with L3_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << '@' << offset.get
    if (dirAccess.isDefined) out << ':' << dirAccess.get
  }

  override def progress = ProgressLocation {
    Logger.warn(s"Trying to progress future stencil field access to $name on level $level")
    L4_FutureStencilFieldAccess(name, level,
      L4_ActiveSlot,
      L3_ProgressOption(offset)(_.progress),
      L3_ProgressOption(dirAccess)(_.progress))
  }

  def toStencilFieldAccess = L3_StencilFieldAccess(this)
}

/// L3_PrepareStencilFieldAccesses

object L3_PrepareStencilFieldAccesses extends DefaultStrategy("Prepare accesses to stencil fields") {
  val collector = new L3_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L3_UnresolvedAccess if L3_StencilFieldCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to stencil field ${ access.name }")
      }

      if (!L3_StencilFieldCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      if (access.slot.isDefined) Logger.warn(s"Discarding meaningless slot access on ${ access.name }")
      if (access.arrayIndex.isDefined) Logger.warn(s"Discarding meaningless array access on ${ access.name }")

      L3_FutureStencilFieldAccess(access.name, lvl, access.offset, access.dirAccess)
  })
}
