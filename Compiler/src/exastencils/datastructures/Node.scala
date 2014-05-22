package exastencils.datastructures

import scala.collection.GenTraversableOnce

trait Node extends Annotatable

final class NodeList(var nodes : GenTraversableOnce[Node])
