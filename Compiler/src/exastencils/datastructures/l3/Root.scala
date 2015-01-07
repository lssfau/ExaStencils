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

  def mkStencilEntry(offset : List[Integer], value : Double) = {
    StaticListRValue(List(
      StaticListRValue(offset map { i => IntegerRValue(i) }),
      FloatRValue(value)))
  }

  def progressToL4 : l4.Root = {
    val env = new Environment
    val program_block = new TcbBlock
    val stMgr = new StencilManager

    val ctx = Context(env, program_block, stMgr)

    /** @todo: Remove this... needed only for testing */
    /* BEGIN */

    val L = StaticListRValue(List(
      mkStencilEntry(List(0, -1), -1.0),
      mkStencilEntry(List(-1, 0), -1.0),
      mkStencilEntry(List(0, 0), 4.0),
      mkStencilEntry(List(1, 0), -1.0),
      mkStencilEntry(List(0, 1), -1.0)))

    env.bind("myL", L)
    env.bind("myR", L)
    env.bind("myF", FieldLValue("myF"))
    env.bind("myU", FieldLValue("myU"))
    env.bind("myDest", FieldLValue("myDest"))
    env.bind("myWork", FieldLValue("myWork"))

    /* END */

    // bind builtin functions to their names
    env.bind("apply", ApplyStencilBuiltin())

    l4.Root(toTc(ctx))
  }

  def toTc(ctx : Context) : List[Node] = {

    // insert functions into the current environment
    functions foreach { _.writeTc(ctx) }

    // instantiate
    functionInstantiations foreach { _.writeTc(ctx) }

    val stm_nodes = ctx.tcb.build
    val stencil_nodes = ctx.stencils.statements

    val tcAst = ListBuffer[Node]()
    tcAst ++= stencil_nodes
    tcAst ++= stm_nodes
    tcAst.toList
  }
}
