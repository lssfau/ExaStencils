package exastencils.datastructures

import scala.collection.mutable.ListBuffer
import exastencils.core.TreeManager

class Strategy {
  protected var trafos = new ListBuffer[Transformation]

  def add(transformation : Transformation) = trafos += transformation
  def +=(transformation : Transformation) = add(transformation)

  def transformations = { trafos.readOnly }

  def apply : Boolean = { TreeManager.defaultApply(this) }
}
