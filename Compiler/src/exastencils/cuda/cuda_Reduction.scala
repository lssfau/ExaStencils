package exastencils.cuda

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_Linearization
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.datastructures.ir.iv
import exastencils.deprecated.ir.IR_DimToString
import exastencils.prettyprinting.PpStream

/// IR_ReductionDeviceDataAccess

case class IR_ReductionDeviceDataAccess(var data : iv.ReductionDeviceData, var index : IR_ExpressionIndex, var strides : IR_ExpressionIndex) extends IR_Expression {
  override def datatype = data.datatype
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def linearize = IR_ArrayAccess(data, IR_Linearization.linearizeIndex(index, strides), alignedAccessPossible = false)
}

/// IR_LinearizeReductionDeviceDataAccess

object IR_LinearizeReductionDeviceDataAccess extends DefaultStrategy("Linearize ReductionDeviceDataAccess nodes") {
  this += new Transformation("Linearize", {
    case access : IR_ReductionDeviceDataAccess => access.linearize
  })
}

/// CUDA_HandleReductions

object CUDA_HandleReductions extends DefaultStrategy("Handle reductions in device kernels") {
  this += new Transformation("Process kernel nodes", {
    case kernel : CUDA_Kernel if kernel.reduction.isDefined =>
      // update assignments according to reduction clauses
      val index = IR_ExpressionIndex((0 until kernel.parallelDims).map(dim =>
        IR_VariableAccess(CUDA_Kernel.KernelVariablePrefix + CUDA_Kernel.KernelGlobalIndexPrefix + IR_DimToString(dim), IR_IntegerDatatype) : IR_Expression).toArray)

      val stride = (kernel.maxIndices, kernel.minIndices).zipped.map((x, y) => IR_SubtractionExpression(x, y) : IR_Expression)

      CUDA_ReplaceReductionAssignments.redTarget = kernel.reduction.get.target.name
      CUDA_ReplaceReductionAssignments.replacement = IR_ReductionDeviceDataAccess(iv.ReductionDeviceData(IR_MultiplicationExpression(ListBuffer[IR_Expression](stride : _*))), index, IR_ExpressionIndex(stride))
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