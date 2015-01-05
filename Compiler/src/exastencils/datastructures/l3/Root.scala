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
    val program_block = new TcbBlock
    val stMgr = new StencilManager

    val ctx = Context(env, program_block, stMgr)

    /// @todo: Remove this... only for testing
    env.bind("myL", StencilRValue())
    env.bind("myR", StencilRValue())
    env.bind("myF", FieldLValue("myF"))
    env.bind("myU", FieldLValue("myU"))
    env.bind("myDest", FieldLValue("myDest"))
    env.bind("myWork", FieldLValue("myWork"))

    // register builtin functions
    env.bind("apply", ApplyStencilBuiltin())

    l4.Root(toTc(ctx))
  }

  def toTc(ctx : Context) : List[l4.Statement] = {

    // insert functions into the current environment
    functions foreach { _.writeTc(ctx) }

    // instantiate
    functionInstantiations foreach { _.writeTc(ctx) }

    ctx.tcb.build
  }
}
