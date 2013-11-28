package exastencils.datastructures

import exastencils.core.TreeManager

class Transformation(f : PartialFunction[Node, Option[Node]], rec : Boolean = true, node : Node = TreeManager.root) {
  def function = f
  def recursive = rec
  def applyAt = node
}
