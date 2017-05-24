package exastencils.field.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_FieldOverride extends Generatable {
  override def validLayers() = ListBuffer(L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/field/|LAYER_LC|/|LAYER_UC|_FieldOverride.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.field.|LAYER_LC|

import exastencils.base.|LAYER_LC|._
import exastencils.boundary.|LAYER_LC|.|LAYER_UC|_BoundaryCondition
import exastencils.datastructures._
import exastencils.logger._
import exastencils.prettyprinting._

/// |LAYER_UC|_OverrideFieldBC

case class |LAYER_UC|_OverrideFieldBC(
    var fieldName : String,
    var levels : Option[|LAYER_UC|_LevelSpecification],
    var newBC : |LAYER_UC|_BoundaryCondition) extends |LAYER_UC|_Statement {

  override def prettyprint(out : PpStream) = {
    out << "override bc of " << fieldName
    if (levels.isDefined) out << "@" << levels.get
    out << "with" << newBC
  }

  override def progress = Logger.error(s"Trying to progress |LAYER_LC| field override statement for field $fieldName; this is not supported")
}

/// |LAYER_UC|_ProcessFieldOverrides

object |LAYER_UC|_ProcessFieldOverrides extends DefaultStrategy("Process field overrides") {
  this += Transformation("Override boundary conditions", {
    case overrideBC : |LAYER_UC|_OverrideFieldBC =>
      val levelList = |LAYER_UC|_LevelSpecification.extractLevelListDefAll(overrideBC.levels)
      for (level <- levelList)
        |LAYER_UC|_FieldCollection.getByIdentifier(overrideBC.fieldName, level).get.boundary = overrideBC.newBC

      None // consume override statement
  })
}
"""
  }
}
