package exastencils.datastructures

import exastencils.core.StateManager

class Transformation(n : String, f : PartialFunction[Node, Option[Node]], rec : Boolean = true, node : Option[Node] = None) {
  def name = n
  def function = f
  def recursive = rec
  def applyAt = node
}

object Transformation {
  def apply(n : String, f : PartialFunction[Node, Option[Node]], rec : Boolean = true, node : Option[Node] = None) =
    new Transformation(n, f, rec, node)
}

class TransformationResult(successful : Boolean, matches : Int, var replacements : Int) {
  override def toString() = {
    var s = "Transformation Result: "
    if (!successful) s += "not "
    s += s"successful, $matches matches, $replacements replacements"
    s
  }
}
