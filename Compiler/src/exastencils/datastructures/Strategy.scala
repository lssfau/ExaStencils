package exastencils.datastructures

import scala.collection.mutable.ListBuffer
import exastencils.core.StateManager

class Strategy(val name: String) {
  protected var trafos = new ListBuffer[Transformation]

  def add(transformation : Transformation) = trafos += transformation
  def +=(transformation : Transformation) = add(transformation)

  def transformations = { trafos.readOnly }

  def apply : Boolean = { StateManager.defaultApply(this) }
}
