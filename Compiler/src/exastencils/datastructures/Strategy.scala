package exastencils.datastructures

import scala.collection.mutable.ListBuffer
import exastencils.core.StateManager

class Strategy(val name : String) {
  protected var trafos = new ListBuffer[Transformation]

  def add(transformation : Transformation) = trafos += transformation
  def +=(transformation : Transformation) = add(transformation)

  def transformations = { trafos.readOnly }

  def apply : Boolean = { StateManager.defaultApply(this) }
}

object Strategy {
  def apply(name : String) = new Strategy(name)
  def apply(name : String, transformations : List[Transformation]) = {
    val s = new Strategy(name)
    s.trafos ++= transformations
    s
  }
}
