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

package exastencils.solver.l1

import exastencils.base.ProgressLocation
import exastencils.baseExt.l1.L1_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l1.L1_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.solver.l2.L2_FutureEquationAccess
import exastencils.util.l1.L1_LevelCollector

/// L1_FutureEquationAccess

case class L1_FutureEquationAccess(var name : String, var level : Int) extends L1_FutureKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << name << '@' << level

  override def progress = ProgressLocation {
    Logger.warn(s"Trying to progress future equation access to $name on level $level")
    L2_FutureEquationAccess(name, level, None)
  }

  def toEquationAccess = L1_EquationAccess(this)
}

/// L1_PrepareEquationAccesses

object L1_PrepareEquationAccesses extends DefaultStrategy("Prepare accesses to equations") {
  val collector = new L1_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L1_UnresolvedAccess if L1_EquationCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to equation ${ access.name }")
      }

      if (!L1_EquationCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      L1_FutureEquationAccess(access.name, lvl)
  })
}
