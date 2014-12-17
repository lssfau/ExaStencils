package exastencils.datastructures.l3

import scala.collection.mutable.ListBuffer
import exastencils.datastructures._

case class Root(var nodes : List[Node]) extends Node with ProgressibleToL4 {
  val functions = ListBuffer[FunctionStatement]()
  val functionInstantiations = ListBuffer[FunctionInstantiationStatement]()

  nodes.foreach(_ match {
    case x : FunctionStatement              => functions += x
    case x : FunctionInstantiationStatement => functionInstantiations += x
  })

  def getFunctionByIdentifier(identifier : String) : Option[FunctionStatement] = {
    functions.find(f => identifier == f.identifier)
  }

  override def progressToL4 : Node = {
    var newRoot = new l4.Root(List())

    //    for (f <- functioninstantiations)
    //      newRoot.statements += f.progressToL4

    newRoot
  }

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
