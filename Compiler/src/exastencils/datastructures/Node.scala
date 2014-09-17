package exastencils.datastructures

import scala.collection.GenTraversableOnce

trait Node extends Annotatable

final class NodeList(var nodes : GenTraversableOnce[Node])
package object ir { /* FIXME: this is only a placeholder until something reasonable is in place */ type StatementList = NodeList }
