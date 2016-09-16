package exastencils.workaround

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.knowledge.Platform

object Compiler extends DefaultStrategy("Compiler workarounds") {
  private val compiler = Platform.targetCompiler
  private val compVerMaj = Platform.targetCompilerVersion
  private val compVerMin = Platform.targetCompilerVersionMinor

  if (compiler == "ICC" && compVerMaj == 16) {
    // icc 16 fails when a method body is too large, so try to break these down into multiple methods
    val threshold : Int = 100
    this += new Transformation("icc 16 internal error (method too large)", new PartialFunction[Node, Transformation.Output[NodeList]] {
      override def isDefinedAt(node : Node) : Boolean = node match {
        case func : IR_Function =>
          func.functionQualifiers.isEmpty &&
            func.parameters.isEmpty &&
            func.body.forall {
              s => s == IR_NullStatement ||
                s.isInstanceOf[IR_Scope] ||
                s.isInstanceOf[IR_Comment] ||
                s.isInstanceOf[IR_Switch] ||
                s.isInstanceOf[IR_IfCondition] ||
                s.isInstanceOf[IR_WhileLoop] ||
                s.isInstanceOf[IR_ForLoop]
            } &&
            func.body.length >= threshold
        case _                  =>
          false
      }

      override def apply(node : Node) : Transformation.Output[NodeList] = {

        val func = node.asInstanceOf[IR_Function]
        var remaining = func.body
        func.body = new ListBuffer[IR_Statement]()
        func.allowInlining = true

        val funcs = new ListBuffer[IR_Function]()
        funcs += func
        var i : Int = 0
        do {
          i += 1
          val newFuncName = func.name + i
          val (pref, rest) = remaining.splitAt(threshold)
          funcs += new IR_Function(func.returntype, newFuncName, new ListBuffer[IR_FunctionArgument](), pref)
          func.body += IR_FunctionCall(newFuncName)
          remaining = rest
        } while (remaining.nonEmpty)
        funcs
      }
    })
  }
}
