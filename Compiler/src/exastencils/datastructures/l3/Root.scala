package exastencils.datastructures.l3

import scala.collection.mutable.ListBuffer
import exastencils.datastructures._

case class Root(var nodes : List[Node]) extends Node with ProgressableToL4 {
  val functions = ListBuffer[FunctionStatement]()
  val functionInstantiations = ListBuffer[FunctionInstantiationStatement]()

  nodes.foreach(_ match {
    case x : FunctionStatement              => functions += x
    case x : FunctionInstantiationStatement => functionInstantiations += x
  })

  def progressToL4 : l4.Root = {
    val env = new Environment

    l4.Root(toTc(env).toList())
  }

  override def toTc(env : Environment) : TargetCode = {

    var tc = new TargetCode(List())

    // insert functions into the current environment
    for (f <- functions) {
      tc = tc ++ f.toTc(env)
    }
    for (inst <- functionInstantiations) {
      tc = tc ++ inst.toTc(env)
    }
    tc
  }
}
