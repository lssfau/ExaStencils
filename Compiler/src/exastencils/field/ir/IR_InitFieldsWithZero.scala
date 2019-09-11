package exastencils.field.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.parallelization.ir.IR_ParallelizationInfo

/// IR_InitFieldsWithZero

case class IR_InitFieldsWithZero() extends IR_FuturePlainFunction {
  override var name = "initFieldsWithZero"
  override def prettyprint_decl() : String = prettyprint

  override def generateFct() = {
    val fields = IR_FieldCollection.sortedObjects
    var statements : ListBuffer[IR_Statement] = new ListBuffer

    for (field <- fields) {
      val numDims = field.fieldLayout.numDimsData
      val index = IR_LoopOverDimensions.defIt(numDims)

      val loopOverDims = new IR_LoopOverDimensions(numDims, IR_ExpressionIndexRange(
        IR_ExpressionIndex((0 until numDims).toArray.map(dim => field.fieldLayout.idxById("GLB", dim))),
        IR_ExpressionIndex((0 until numDims).toArray.map(dim => field.fieldLayout.idxById("GRE", dim)))),
        (0 until field.numSlots).to[ListBuffer].map(slot =>
          IR_Assignment(
            IR_DirectFieldAccess(field, slot, Duplicate(index)),
            0.0) : IR_Statement))
      loopOverDims.parallelization.potentiallyParallel = true
      loopOverDims.polyOptLevel = 1

      val wrapped = IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(field.domain.index), loopOverDims),
        IR_ParallelizationInfo(potentiallyParallel = true))

      if ("MSVC" == Platform.targetCompiler /*&& Platform.targetCompilerVersion <= 11*/ ) // fix for https://support.microsoft.com/en-us/kb/315481
        statements += IR_Scope(wrapped)
      else
        statements += wrapped
    }

    IR_PlainFunction(name, IR_UnitDatatype, statements)
  }
}
