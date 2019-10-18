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

package exastencils.field.l4

import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l4.L4_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l4.L4_LevelCollector

/// L4_FutureFieldLayoutAccess

case class L4_FutureFieldLayoutAccess(var name : String, var level : Int) extends L4_FutureKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << name << '@' << level
  override def progress = Logger.error(s"Trying to progress future field layout access to $name on level $level")
  def toFieldLayoutAccess = L4_FieldLayoutAccess(this)
}

/// L4_PrepareFieldLayoutAccesses

object L4_PrepareFieldLayoutAccesses extends DefaultStrategy("Prepare accesses to field layouts") {
  val collector = new L4_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L4_UnresolvedAccess if L4_FieldLayoutCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to field layout ${ access.name }")
      }

      if (access.offset.isDefined) Logger.warn("Discarding meaningless offset access on field layout")
      if (access.dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on field layout")

      if (!L4_FieldLayoutCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      L4_FutureFieldLayoutAccess(access.name, lvl)
  })
}
