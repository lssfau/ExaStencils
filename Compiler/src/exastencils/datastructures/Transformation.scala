package exastencils.datastructures

import exastencils.core.StateManager

class Transformation(f : PartialFunction[Node, Option[Node]], rec : Boolean = true, node : Node = StateManager.root) {
  def function = f
  def recursive = rec
  def applyAt = node
}

object Transformation {
  def apply(f : PartialFunction[Node, Option[Node]], rec : Boolean = true, node : Node = StateManager.root) =
    new Transformation(f, rec, node)
}

case class TransformationException(msg : String) extends RuntimeException(msg)
