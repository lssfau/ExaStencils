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

package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting._
import exastencils.util.l4.L4_LevelCollector

/// L4_LevelScope

object L4_LevelScope {
  def apply(level : L4_LevelSpecification, body : List[L4_Statement]) = new L4_LevelScope(level, body.to[ListBuffer])
}

case class L4_LevelScope(var level : L4_LevelSpecification, var body : ListBuffer[L4_Statement]) extends L4_Statement {
  override def prettyprint(out : PpStream) = out << "@" << level << " {\n" <<< (body, "\n") << "\n}"
  override def progress = Logger.error("Trying to progress " + this.getClass.getName + " which is unsupported")
}

/// L4_ResolveLevelScopes

object L4_ResolveLevelScopes extends DefaultStrategy("Resolve level-specific scopes") {
  var levelCollector = new L4_LevelCollector
  this.register(levelCollector)
  this.onBefore = () => this.resetCollectors()

  // Flatten leveled scope or remove completely
  this += new Transformation("Resolve", {
    case scope : L4_LevelScope => scope.level match {
      case s : L4_SingleLevel =>
        if (levelCollector.getCurrentLevel == s.level)
          scope.body
        else
          List()
      case s : L4_LevelList   =>
        if (s.contains(levelCollector.getCurrentLevel))
          scope.body
        else
          List()
    }
  })
}
