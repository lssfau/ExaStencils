package exastencils.datastructures.l3

import scala.collection.mutable.ListBuffer
import exastencils.datastructures._

case class Root(var nodes : List[Node]) extends Node {
  val functionDefinitions = ListBuffer[FunctionStatement]()
  val functionInstantiations = ListBuffer[FunctionInstantiationStatement]()
  val staticAssignments = ListBuffer[StaticAssignmantStatement]()

  nodes.foreach(_ match {
    case x : FunctionStatement              => functionDefinitions += x
    case x : FunctionInstantiationStatement => functionInstantiations += x
    case x : StaticAssignmantStatement      => staticAssignments += x
    case _                                  => throw new Exception("Unexpected node in program AST")
  })

  def progressToL4 : l4.Root = {
    val env = new Environment
    val program_block = new TcbBlock
    val stMgr = new StencilManager

    val ctx = Context(env, program_block, stMgr)

    /** @todo: Remove this... needed only for testing */
    /* BEGIN */
    env.bind("myF", FieldLValue("myF"))
    env.bind("myU", FieldLValue("myU"))
    env.bind("myDest", FieldLValue("myDest"))
    env.bind("myWork", FieldLValue("myWork"))

    /* END */

    // bind builtin functions to their names
    env.bind("apply", ApplyStencilBuiltin())
    env.bind("diag_inv", DiagInvBuiltin())

    l4.Root(toTc(ctx))
  }

  def toTc(ctx : Context) : List[Node] = {

    // perform static assignments
    staticAssignments foreach { _.writeTc(ctx) }

    // insert functions into the current environment
    functionDefinitions foreach { _.writeTc(ctx) }

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
