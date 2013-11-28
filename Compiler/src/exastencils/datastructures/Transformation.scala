package exastencils.datastructures

class Transformation(f : PartialFunction[Node, Option[Node]]) {
  def apply(node : Node) : Option[Node] = {
    if (f.isDefinedAt(node)) return f(node) else return Some(node)
  }

  def stop : Boolean = { return false }
  def stopAfterNode : Boolean = { return false }
}

