package exastencils.field.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.datastructures.Transformation._
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.parallelization.ir.IR_ParallelizationInfo
import exastencils.polyhedron.PolyhedronAccessible

/// IR_InitFieldsWithZero

case class IR_InitFieldsWithZero() extends IR_AbstractFunction with IR_Expandable {
  override def prettyprint_decl() : String = prettyprint
  override def name = "initFieldsWithZero"

  override def expand() : Output[IR_Function] = {
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
            IR_DirectFieldAccess(IR_FieldSelection(field, field.level, slot), index),
            0.0) : IR_Statement)) with PolyhedronAccessible
      loopOverDims.parallelization.potentiallyParallel = true
      loopOverDims.optLevel = 1

      val wrapped = IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(field.domain.index), loopOverDims),
        IR_ParallelizationInfo.PotentiallyParallel())

      if ("MSVC" == Platform.targetCompiler /*&& Platform.targetCompilerVersion <= 11*/ ) // fix for https://support.microsoft.com/en-us/kb/315481
        statements += IR_Scope(wrapped)
      else
        statements += wrapped
    }

    IR_Function(IR_UnitDatatype, name, statements)
  }
}
