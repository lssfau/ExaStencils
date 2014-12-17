package exastencils.datastructures.l3

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l3._
import exastencils.knowledge._
import exastencils.polyhedron._
import scala.collection.mutable.ListBuffer

case class Root(var nodes : List[Node]) extends Node with ProgressableToL4 {
  var functions = ListBuffer[FunctionStatement]()
  var functioninstantiations = ListBuffer[FunctionInstantiationStatement]()

  nodes.foreach(_ match {
    case x : FunctionStatement              => functions += x
    case x : FunctionInstantiationStatement => functioninstantiations += x
  })

  override def progressToL4 : Node = {
    new l4.Root(List())
  }

  override def toDc(env : Environment) : DestinationCode = {

    ???
  }
}
