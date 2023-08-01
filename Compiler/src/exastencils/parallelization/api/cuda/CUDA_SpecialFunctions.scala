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

package exastencils.parallelization.api.cuda

import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.datastructures._
import exastencils.scheduling.NoStrategyWrapper
import exastencils.util.ir.IR_Print

/// CUDA_ReplaceStdFunctionCalls

object CUDA_ReplaceStdFunctionCalls extends DefaultStrategy("Replace calls to standard functions in device kernels") {
  var addedFunctions = HashSet[String]()

  this += new Transformation("Process kernel nodes", {
    case fctCall : IR_FunctionCall if CUDA_StdFunctionReplacements.stdFunctions.contains(fctCall.name) =>
      val cudaFctName = CUDA_StdFunctionReplacements.stdFunctions(fctCall.name)
      if (!addedFunctions.contains(cudaFctName)) {
        CUDA_StdFunctionReplacements.addReplacement(cudaFctName)
        addedFunctions += cudaFctName
      }

      fctCall.function = IR_PlainInternalFunctionReference(cudaFctName, IR_UnitDatatype)
      fctCall
  })

  this += new Transformation("Remove print functions in device kernels", {
    case _ : IR_Print => None
  })
}

/// CUDA_StdFunctionReplacements

object CUDA_StdFunctionReplacements {
  val stdFunctions = HashMap[String, String]("std::fill" -> "cuda_std_fill", "std::copy" -> "cuda_std_copy", "std::swap" -> "cuda_std_swap")

  def addFct(fct : IR_Function) {
    fct.allowInlining = false
    fct.allowFortranInterface = false
    fct.functionQualifiers = "inline __device__"
    fct.isHeaderOnly = true

    fct.annotate("deviceOnly")

    CUDA_KernelFunctions.get.functions += fct
  }

  def addReplacement(name : String) {
    name match {
      case "cuda_std_fill" =>
        // std::fill(first, last, value)
        val dt = if (Knowledge.useDblPrecision) IR_DoubleDatatype else IR_FloatDatatype
        def first = IR_VariableAccess("first", IR_PointerDatatype(dt))
        def last = IR_VariableAccess("last", IR_PointerDatatype(dt))
        def value = IR_VariableAccess("value", dt)
        addFct(
          IR_PlainFunction(name, IR_UnitDatatype, ListBuffer(IR_FunctionArgument(first), IR_FunctionArgument(last), IR_FunctionArgument(value)),
            IR_WhileLoop(IR_Neq(first, last),
              IR_Assignment(IR_DerefAccess(first), value),
              IR_PreIncrement(first))))

      case "cuda_std_copy" =>
        // std::copy(first, last, d_first)
        val dt = if (Knowledge.useDblPrecision) IR_DoubleDatatype else IR_FloatDatatype
        def first = IR_VariableAccess("first", IR_PointerDatatype(dt))
        def last = IR_VariableAccess("last", IR_PointerDatatype(dt))
        def d_first = IR_VariableAccess("d_first", IR_PointerDatatype(dt))
        addFct(
          IR_PlainFunction(name, IR_PointerDatatype(dt), ListBuffer(IR_FunctionArgument(first), IR_FunctionArgument(last), IR_FunctionArgument(d_first)),
            ListBuffer(
              IR_WhileLoop(IR_Neq(first, last),
                IR_Assignment(IR_DerefAccess(d_first), IR_DerefAccess(first)),
                IR_PreIncrement(d_first),
                IR_PreIncrement(first)),
              IR_Return(d_first))))

      case "cuda_std_swap" =>
        // std::swap
        for (dt <- List(IR_IntegerDatatype, IR_FloatDatatype, IR_DoubleDatatype)) {
          def nju = IR_VariableAccess("nju", dt)
          def left = IR_VariableAccess("left", IR_ReferenceDatatype(dt))
          def right = IR_VariableAccess("right", IR_ReferenceDatatype(dt))
          addFct(IR_PlainFunction(name, IR_UnitDatatype, ListBuffer(IR_FunctionArgument(left), IR_FunctionArgument(right)),
            ListBuffer(
              IR_VariableDeclaration(nju, left),
              IR_Assignment(left, right),
              IR_Assignment(right, nju))))
        }
    }
  }
}

/// CUDA_ReplaceStdFunctionCallsWrapper

object CUDA_ReplaceStdFunctionCallsWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
    if (Knowledge.cuda_enabled)
      CUDA_ReplaceStdFunctionCalls.apply(Some(CUDA_KernelFunctions.get))
  }
}
