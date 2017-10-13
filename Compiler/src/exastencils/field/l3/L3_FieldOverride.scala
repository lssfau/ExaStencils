package exastencils.field.l3

import exastencils.base.l3._
import exastencils.boundary.l3.L3_BoundaryCondition
import exastencils.datastructures._
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
        L3_FieldCollection.getByIdentifier(overrideBC.fieldName, level).get.boundary = overrideBC.newBC

      None // consume override statement
  })
}
