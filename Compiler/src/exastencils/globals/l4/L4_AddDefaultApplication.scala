package exastencils.globals.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ExaRootNode
import exastencils.base.l4.L4_ImplicitConversion._
import exastencils.base.l4._
import exastencils.core.StateManager
import exastencils.datastructures._

/// L4_AddDefaultApplication

object L4_AddDefaultApplication extends DefaultStrategy("") {
  override def apply(node : Option[Node] = None) : Unit = {
    if (StateManager.findFirst[L4_FunctionDecl]({ f : L4_FunctionDecl => f.name == "Application" }).isDefined)
      return // Application is already defined

    ExaRootNode.l4_root.nodes += generateFunction()

    super.apply(node)
  }

  def generateFunction() = {
    val stmts = ListBuffer[L4_Statement]()

    def fctCall(fctName : String, args : ListBuffer[L4_Expression]) = L4_FunctionCall(L4_UnresolvedFunctionReference(fctName, None, None), args)
    def startTimer(name : String) = fctCall("startTimer", ListBuffer[L4_Expression](L4_StringConstant(name)))
    def stopTimer(name : String) = fctCall("stopTimer", ListBuffer[L4_Expression](L4_StringConstant(name)))

    // init
    stmts += startTimer("setup")
    stmts += fctCall("initGlobals", ListBuffer())
    stmts += fctCall("initDomain", ListBuffer())
    stmts += fctCall("initFieldsWithZero", ListBuffer())
    stmts += fctCall("initGeometry", ListBuffer())
    stmts += fctCall("InitFields", ListBuffer())
    stmts += stopTimer("setup")

    // solve
    stmts += startTimer("solve")
    stmts += L4_FunctionCall(L4_UnresolvedFunctionReference("gen_solve", Some(L4_FinestLevel), None), ListBuffer[L4_Expression]())
    stmts += stopTimer("solve")

    // de-init
    stmts += fctCall("printAllTimers", ListBuffer())
    stmts += fctCall("destroyGlobals", ListBuffer())

    L4_FunctionDecl("Application", None, L4_UnitDatatype, ListBuffer[L4_Function.Argument](), stmts, false)
  }
}