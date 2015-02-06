package exastencils.datastructures.l3

import scala.collection.mutable.ListBuffer
import exastencils.datastructures._

case class Root(var nodes : List[Node]) extends Node {
  val program_statements = ListBuffer[Statement]()

  nodes.foreach(_ match {
    case x : Statement => program_statements += x
    case _             => throw new Exception("Unexpected node in program AST")
  })

  def progressToL4 : l4.Root = {
    val env = new Environment
    val program_block = new TcbBlock
    val stMgr = new StencilManager
    val fldMgr = new FieldManager

    val ctx = Context(env, program_block, stMgr, fldMgr)

    // bind builtin functions to their names
    env.bind("return", StaticConstant(ReturnBuiltin()))
    env.bind("apply", StaticConstant(ApplyStencilBuiltin()))
    env.bind("diag_inv", StaticConstant(DiagInvBuiltin()))
    env.bind("field", StaticConstant(InstantiateFieldBuiltin()))
    env.bind("list_append", StaticConstant(ListAppendBuiltin()))

    l4.Root(toTc(ctx))
  }

  def toTc(ctx : Context) : List[Node] = {

    program_statements foreach { n =>
      n match {
        case f : FunctionCallStatement => f.exec(ctx)
        case _                         => n.writeTc(ctx)
      }
    }

    val stm_nodes = ctx.tcb.build
    val field_nodes = ctx.fields.statements
    val stencil_nodes = ctx.stencils.statements

    val tcAst = ListBuffer[Node]()
    tcAst ++= field_nodes
    tcAst ++= stencil_nodes
    tcAst ++= stm_nodes
    tcAst.toList
  }
}
