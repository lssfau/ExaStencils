//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.workaround

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Platform
import exastencils.datastructures._

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
              s =>
                s == IR_NullStatement ||
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
        func.body = ListBuffer[IR_Statement]()
        func.allowInlining = true

        val funcs = ListBuffer[IR_Function]()
        funcs += func
        var i : Int = 0
        do {
          i += 1
          val newFuncName = func.name + i
          val (pref, rest) = remaining.splitAt(threshold)
          funcs += IR_PlainFunction(newFuncName, func.datatype, ListBuffer[IR_FunctionArgument](), pref)
          func.body += IR_FunctionCall(newFuncName)
          remaining = rest
        } while (remaining.nonEmpty)
        funcs
      }
    })
  }
}
