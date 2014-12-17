package exastencils.datastructures.l3

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l3._
import exastencils.knowledge._
import exastencils.polyhedron._
import scala.collection.mutable.ListBuffer

case class Root(var nodes : List[Node]) extends Node with ProgressibleToL4 {
  val functions = ListBuffer[FunctionStatement]()
  val functionInstantiations = ListBuffer[FunctionInstantiationStatement]()

  nodes.foreach(_ match {
    case x : FunctionStatement              => functions += x
    case x : FunctionInstantiationStatement => functionInstantiations += x
  })

  override def toDc(env : Environment) : DestinationCode = {

    var dc = new DestinationCode(List())

    // insert functions into the current environment
    for (f <- functions) {
      dc = dc ++ f.toDc(env)
    }
    for (inst <- functionInstantiations) {
      dc = dc ++ inst.toDc(env)
    }

    dc
  }
}
