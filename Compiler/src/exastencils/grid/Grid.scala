package exastencils.grid

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.logger._

// helper object/method to branch grid types
object Grid {
  // shortcut functions
  def getGeometry = GridGeometry.getGeometry
  def getEvaluator = GridEvaluator.getEvaluator

  // strategies
  def applyStrategies() = {
    ResolveEvaluationFunctions.apply()
    ResolveIntegrationFunctions.apply()
    ExpandEvaluationFunctions.apply()

    ResolveVirtualFields.apply()
  }
}
