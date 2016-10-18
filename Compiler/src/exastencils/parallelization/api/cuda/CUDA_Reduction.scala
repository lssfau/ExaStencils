package exastencils.parallelization.api.cuda

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.deprecated.ir.IR_DimToString

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

case class CUDA_ReductionDeviceData(var size : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) {
  override def resolveDatatype() = IR_PointerDatatype(IR_RealDatatype)
  // TODO: extend for other types
  override def resolveName() : String = "reductionDeviceData" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")

  override def getDtor() : Option[IR_Statement] = {
    val access = resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt)
    Some(IR_IfCondition(access,
      ListBuffer[IR_Statement](
        CUDA_Free(access),
        IR_Assignment(access, 0))))
  }
}

/// CUDA_HandleReductions

object CUDA_HandleReductions extends DefaultStrategy("Handle reductions in device kernels") {
  this += new Transformation("Process kernel nodes", {
    case kernel : CUDA_Kernel if kernel.reduction.isDefined =>
      // update assignments according to reduction clauses
      val index = IR_ExpressionIndex((0 until kernel.parallelDims).map(dim =>
        IR_VariableAccess(CUDA_Kernel.KernelVariablePrefix + CUDA_Kernel.KernelGlobalIndexPrefix + IR_DimToString(dim), IR_IntegerDatatype) : IR_Expression).toArray)

      val stride = (kernel.maxIndices, kernel.minIndices).zipped.map((x, y) => IR_Subtraction(x, y) : IR_Expression)

      CUDA_ReplaceReductionAssignments.redTarget = kernel.reduction.get.target.name
      CUDA_ReplaceReductionAssignments.replacement = CUDA_ReductionDeviceDataAccess(CUDA_ReductionDeviceData(IR_Multiplication(ListBuffer[IR_Expression](stride : _*))), index, IR_ExpressionIndex(stride))
      CUDA_ReplaceReductionAssignments.applyStandalone(IR_Scope(kernel.body))
      kernel
  })

  /// CUDA_ReplaceReductionAssignments

  object CUDA_ReplaceReductionAssignments extends QuietDefaultStrategy("Replace assignments to reduction targets") {
    var redTarget : String = ""
    var replacement : IR_Expression = IR_NullExpression

    this += new Transformation("Replace", {
      case assignment : IR_Assignment =>
        assignment.dest match {
          case va : IR_VariableAccess if redTarget.equals(va.name) =>
            assignment.dest = Duplicate(replacement)
          // assignment.op = "=" // don't modify assignments - there could be inlined loops
          case _ =>
        }
        assignment
    })
  }

}