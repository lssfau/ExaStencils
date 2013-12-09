package exastencils.datastructures.l4

import exastencils.datastructures._

case class Root(var statements : List[Statement]) extends Node {
	override def duplicate = { this.copy(statements = Duplicate(statements)).asInstanceOf[this.type] }
}