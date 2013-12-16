package exastencils.datastructures.ir

import exastencils.datastructures._
import scala.collection.mutable.ListBuffer

case class Root(var statements : List[Node]) extends Node {
	override def duplicate = { //this.copy(statements=ListBuffer()).asInstanceOf[this.type]
	this.copy(statements = Duplicate(statements)).asInstanceOf[this.type]
	}
}
