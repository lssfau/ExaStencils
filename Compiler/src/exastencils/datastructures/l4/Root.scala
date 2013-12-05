package exastencils.datastructures.l4

import exastencils.datastructures._

case class Root(statements : List[Statement]) extends Node with CppPrettyPrintable {
	override def cpp = statements.map(f => f.cpp).mkString("\n")
	override def duplicate = { this.copy(statements = Duplicate(statements)).asInstanceOf[this.type] }
}