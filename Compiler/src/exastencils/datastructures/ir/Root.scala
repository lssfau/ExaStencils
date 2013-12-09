package exastencils.datastructures.ir

import exastencils.datastructures._
import scala.collection.mutable.ListBuffer

case class Root(var statements : List[Statement]) extends Node with CppPrettyPrintable {
	override def cpp = statements.map(f => f.cpp).mkString("\n")
	override def duplicate = { //this.copy(statements=ListBuffer()).asInstanceOf[this.type]
	this.copy(statements = Duplicate(statements)).asInstanceOf[this.type]
	}
}
