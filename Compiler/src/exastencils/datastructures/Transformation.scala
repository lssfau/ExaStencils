package exastencils.datastructures

import exastencils.core.StateManager

class Transformation(n : String, f : PartialFunction[Node, Option[Node]], rec : Boolean = true, node : Node = StateManager.root) {
  def name = n
  def function = f
  def recursive = rec
  def applyAt = node
}

object Transformation {
  def apply(n : String, f : PartialFunction[Node, Option[Node]], rec : Boolean = true, node : Node = StateManager.root) =
    new Transformation(n, f, rec, node)
}

class TransformationResult(successful : Boolean, matches : Int, replacements : Int)
