package exastencils.datastructures.l3

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._

case class Root(var nodes : List[Node]) extends Node with ProgressableToL4 {
  var functions = ListBuffer[FunctionStatement]()
  var functioninstantiations = ListBuffer[FunctionInstantiationStatement]()

  nodes.foreach(_ match {
    case x : FunctionStatement              => functions += x
    case x : FunctionInstantiationStatement => functioninstantiations += x
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

    ???
  }
}
