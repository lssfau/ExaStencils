package exastencils.workaround

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.datastructures.ir._
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
        case func : FunctionStatement =>
          return func.functionQualifiers.isEmpty &&
            func.parameters.isEmpty &&
            func.body.forall {
              s => s == IR_NullStatement ||
                s.isInstanceOf[Scope] ||
                s.isInstanceOf[CommentStatement] ||
                s.isInstanceOf[SwitchStatement] ||
                s.isInstanceOf[ConditionStatement] ||
                s.isInstanceOf[WhileLoopStatement] ||
                s.isInstanceOf[ForLoopStatement]
            } &&
            func.body.length >= threshold
        case _                        =>
          return false
      }

      override def apply(node : Node) : Transformation.Output[NodeList] = {

        val func = node.asInstanceOf[FunctionStatement]
        var remaining = func.body
        func.body = new ListBuffer[IR_Statement]()
        func.allowInlining = true

        val funcs = new ListBuffer[FunctionStatement]()
        funcs += func
        var i : Int = 0
        do {
          i += 1
          val newFuncName = func.name + i
          val (pref, rest) = remaining.splitAt(threshold)
          funcs += new FunctionStatement(func.returntype, newFuncName, new ListBuffer[FunctionArgument](), pref)
          func.body += new IR_ExpressionStatement(new FunctionCallExpression(newFuncName))
          remaining = rest
        } while (!remaining.isEmpty)
        return funcs
      }
    })
  }
}
