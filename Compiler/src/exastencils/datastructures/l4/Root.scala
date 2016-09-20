package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.globals._
import exastencils.multiGrid._
import exastencils.prettyprinting._

case class Root()(nodes : List[Node]) extends Node with L4_Progressable with PrettyPrintable {

  var globals : ListBuffer[GlobalDeclarationStatement] = new ListBuffer()
  var functions : ListBuffer[L4_Function] = new ListBuffer()
  var statements : ListBuffer[L4_Statement] = new ListBuffer()
  var otherNodes : ListBuffer[L4_Node] = new ListBuffer()

  nodes.foreach {
    case p : GlobalDeclarationStatement => globals.+=(p)
    case p : L4_Function                => functions.+=(p)
    case p : L4_Statement               => statements.+=(p)
    case r : Root                       =>
      globals.++=(r.globals)
      functions.++=(r.functions)
      statements.++=(r.statements)
      otherNodes.++=(r.otherNodes)
    case p : L4_Node                    => otherNodes.+=(p)
  }

  override def prettyprint(out : PpStream) : Unit = {
    // print L4_DomainCollection, L4_FieldLayoutCollection, L4_FieldCollection, L4_StencilCollection, L4_StencilFieldCollection

    if (!globals.isEmpty)
      out <<< globals << '\n'
    if (!functions.isEmpty)
      out <<< (functions, "\n") << '\n'
    if (!statements.isEmpty)
      out <<< (statements, "\n") << '\n'
  }

  override def progress : IR_Root = {
    var newRoot = IR_Root()

    // Globals
    var progGlobals = new Globals(new ListBuffer)
    globals.foreach(f => progGlobals.variables ++= f.progress)
    newRoot += progGlobals

    var multiGrid = new MultiGridFunctions // FIXME: think about how to manage (MG/other) functions
    functions.foreach(multiGrid.functions += _.progress)
    newRoot += multiGrid

    newRoot
  }
}
