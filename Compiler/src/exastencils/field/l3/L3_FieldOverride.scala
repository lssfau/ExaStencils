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

package exastencils.field.l3

import exastencils.base.l3._
import exastencils.boundary.l3.L3_BoundaryCondition
import exastencils.datastructures._
import exastencils.fieldlike.l3.L3_FieldLikeCollections
import exastencils.logger._
import exastencils.prettyprinting._

/// L3_OverrideFieldBC

case class L3_OverrideFieldBC(
    var fieldName : String,
    var levels : Option[L3_LevelSpecification],
    var newBC : L3_BoundaryCondition) extends L3_Statement {

  override def prettyprint(out : PpStream) = {
    out << "override bc of " << fieldName
    if (levels.isDefined) out << "@" << levels.get
    out << " with " << newBC
  }

  override def progress = Logger.error(s"Trying to progress l3 field override statement for field $fieldName; this is not supported")
}

/// L3_ProcessFieldOverrides

object L3_ProcessFieldOverrides extends DefaultStrategy("Process field overrides") {
  this += Transformation("Override boundary conditions", {
    case overrideBC : L3_OverrideFieldBC =>
      val levelList = L3_LevelSpecification.extractLevelListDefAll(overrideBC.levels)
      for (level <- levelList)
        L3_FieldLikeCollections.getByIdentifier(overrideBC.fieldName, level).get.boundary = overrideBC.newBC

      None // consume override statement
  })
}
