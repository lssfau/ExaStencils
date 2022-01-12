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

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// CUDA_ReductionDeviceDataAccess

case class CUDA_ReductionDeviceDataAccess(var data : CUDA_ReductionDeviceData, var index : IR_ExpressionIndex, var strides : IR_ExpressionIndex) extends IR_Expression with IR_SpecialExpandable {
  override def datatype = data.datatype

  def linearize = IR_ArrayAccess(data, IR_Linearization.linearizeIndex(index, strides), alignedAccessPossible = false)
}

/// CUDA_LinearizeReductionDeviceDataAccess

object CUDA_LinearizeReductionDeviceDataAccess extends DefaultStrategy("Linearize ReductionDeviceDataAccess nodes") {
  this += new Transformation("Linearize", {
    case access : CUDA_ReductionDeviceDataAccess => access.linearize
  })
}

/// CUDA_ReductionDeviceData

case class CUDA_ReductionDeviceData(var size : IR_Expression, var targetDt : IR_Datatype, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)
  def baseDt = targetDt.resolveBaseDatatype

  override def resolveDatatype() = IR_PointerDatatype(baseDt)

  override def resolveName() : String = "reductionDeviceData" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")

  override def getDtor() : Option[IR_Statement] = {
    val access = resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt)
    Some(IR_LoopOverFragments(
      IR_IfCondition(access,
        ListBuffer[IR_Statement](
          CUDA_Free(access),
          IR_Assignment(access, 0)))))
  }
}

/// CUDA_HandleReductions

object CUDA_HandleReductions extends DefaultStrategy("Handle reductions in device kernels") {
  this += new Transformation("Process kernel nodes", {
    case kernel : CUDA_Kernel if kernel.reduction.isDefined =>
      val target = kernel.reduction.get.target
      val resultDt = CUDA_Util.getReductionDatatype(target)
      val strideHodt = resultDt.getSizeArray.product

      // update assignments according to reduction clauses
      val index = IR_ExpressionIndex((0 until kernel.parallelDims).map(dim =>
        strideHodt * IR_VariableAccess(CUDA_Kernel.KernelVariablePrefix + CUDA_Kernel.KernelGlobalIndexPrefix + dim, IR_IntegerDatatype)
          - IR_IntegerConstant(kernel.minIndices(dim)) : IR_Expression).toArray)

      val size = IR_IntegerConstant(1)
      val l : Int = kernel.maxIndices.length
      val stride = IR_ExpressionIndex(new Array[IR_Expression](l))
      for (i <- 0 until l) {
        val s = kernel.maxIndices(i) - kernel.minIndices(i)
        size.v *= s
        stride(i) = IR_IntegerConstant(s)
      }

      CUDA_ReplaceReductionAssignments.redTarget = target
      CUDA_ReplaceReductionAssignments.replacement = CUDA_ReductionDeviceDataAccess(CUDA_ReductionDeviceData(size, resultDt), index, stride)
      CUDA_ReplaceReductionAssignments.applyStandalone(IR_Scope(kernel.body))
      kernel
  })

  /// CUDA_ReplaceReductionAssignments

  object CUDA_ReplaceReductionAssignments extends QuietDefaultStrategy("Replace assignments to reduction targets") {
    var redTarget : IR_Expression = IR_NullExpression
    var replacement : IR_Expression = IR_NullExpression

    this += new Transformation("Replace", {
      // replace directly if expr == redTarget
      case assignment @ IR_Assignment(expr, _, _) if redTarget.equals(expr) =>
        assignment.dest = Duplicate(replacement)
        // assignment.op = "=" // don't modify assignments - there could be inlined loops
        assignment

      // array access of reduction target
      case assignment @ IR_Assignment(_ @ IR_ArrayAccess(base : IR_VariableAccess, idx, _), _, _) if redTarget.equals(base) =>
        val repl = Duplicate(replacement) match {
          case red : CUDA_ReductionDeviceDataAccess =>
            red.index(0) += idx
            red
          case _ => Logger.error("Invalid type for \"replacement\" of cuda reduction targets")
        }
        assignment.dest = repl
        // assignment.op = "=" // don't modify assignments - there could be inlined loops
        assignment

      // special functions used for certain kinds of matrix assignments
      case stmt @ IR_ExpressionStatement(IR_FunctionCall(ref @ IR_ExternalFunctionReference(name, IR_UnitDatatype), args @ ListBuffer(_, _, dest))) =>
        if (CUDA_StdFunctionReplacements.stdFunctions.contains(name) && redTarget.equals(dest))
          IR_ExpressionStatement(IR_FunctionCall(ref, args.dropRight(1) :+ IR_AddressOf(replacement))) // replace dest
        else
          stmt
    })
  }

}
