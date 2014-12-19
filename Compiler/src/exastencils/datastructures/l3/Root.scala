package exastencils.datastructures.l3

import scala.collection.mutable.ListBuffer
import exastencils.datastructures._

case class Root(var nodes : List[Node]) extends Node {
  val functions = ListBuffer[FunctionStatement]()
  val functionInstantiations = ListBuffer[FunctionInstantiationStatement]()

  nodes.foreach(_ match {
    case x : FunctionStatement              => functions += x
    case x : FunctionInstantiationStatement => functionInstantiations += x
  })

  def progressToL4 : l4.Root = {
    val env = new Environment

    /// @todo: Remove this... only for testing
    env.bind("myL", Environment.StaticValueItem(StencilRValue()))
    env.bind("myF", Environment.StaticValueItem(FieldLValue("myF")))
    env.bind("myU", Environment.StaticValueItem(FieldLValue("myU")))
    env.bind("myDest", Environment.StaticValueItem(FieldLValue("myDest")))
    env.bind("myWork", Environment.StaticValueItem(FieldLValue("myWork")))

    l4.Root(toTc(env))
  }

  def toTc(env : Environment) : List[l4.Statement] = {

    val program_block = new TcbBlock

    // insert functions into the current environment
    functions foreach { _.writeTc(env, program_block) }

    // instantiate
    functionInstantiations foreach { _.writeTc(env, program_block) }

    program_block.build
  }
}
