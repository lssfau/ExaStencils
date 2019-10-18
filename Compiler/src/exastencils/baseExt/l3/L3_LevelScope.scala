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

package exastencils.baseExt.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.baseExt.l4.L4_LevelScope
import exastencils.datastructures._
import exastencils.prettyprinting._
import exastencils.util.l3.L3_LevelCollector

/// L3_LevelScope

object L3_LevelScope {
  def apply(level : L3_LevelSpecification, body : List[L3_Statement]) = new L3_LevelScope(level, body.to[ListBuffer])
}

case class L3_LevelScope(var level : L3_LevelSpecification, var body : ListBuffer[L3_Statement]) extends L3_Statement {
  override def prettyprint(out : PpStream) = out << "@" << level << " {\n" <<< (body, "\n") << "\n}"
  override def progress = ProgressLocation(L4_LevelScope(level.progress, body.map(_.progress)))
}

/// L3_ResolveLevelScopes

object L3_ResolveLevelScopes extends DefaultStrategy("Resolve level-specific scopes") {
  var levelCollector = new L3_LevelCollector
  this.register(levelCollector)

  // Flatten leveled scope or remove completely
  this += new Transformation("Resolve", {
    case scope : L3_LevelScope => scope.level match {
      case s : L3_SingleLevel =>
        if (levelCollector.getCurrentLevel == s.level)
          scope.body
        else
          List()
      case s : L3_LevelList   =>
        if (s.contains(levelCollector.getCurrentLevel))
          scope.body
        else
          List()
    }
  })
}
