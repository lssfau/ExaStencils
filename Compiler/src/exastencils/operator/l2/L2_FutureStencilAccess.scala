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
import exastencils.baseExt.l2.L2_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l2.L2_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.operator.l3.L3_FutureStencilAccess
import exastencils.prettyprinting.PpStream
import exastencils.util.l2.L2_LevelCollector

/// L2_FutureStencilAccess

case class L2_FutureStencilAccess(
    var name : String, var level : Int,
    var offset : Option[L2_ConstIndex],
    var dirAccess : Option[L2_ConstIndex]) extends L2_FutureKnowledgeAccess with L2_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << '@' << offset.get
    if (dirAccess.isDefined) out << ':' << dirAccess.get
  }

  override def progress = ProgressLocation {
    Logger.warn(s"Trying to progress future stencil access to $name on level $level")
    L3_FutureStencilAccess(name, level,
      L2_ProgressOption(offset)(_.progress),
      L2_ProgressOption(dirAccess)(_.progress))
  }

  def toStencilAccess = L2_StencilAccess(this)
}

/// L2_PrepareStencilAccesses

object L2_PrepareStencilAccesses extends DefaultStrategy("Prepare accesses to stencils") {
  val collector = new L2_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L2_UnresolvedAccess if L2_StencilCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to stencil ${ access.name }")
      }

      if (!L2_StencilCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      if (access.slot.isDefined) Logger.warn(s"Discarding meaningless slot access on ${ access.name }")
      if (access.arrayIndex.isDefined) Logger.warn(s"Discarding meaningless array access on ${ access.name }")

      L2_FutureStencilAccess(access.name, lvl, access.offset, access.dirAccess)
  })
}
