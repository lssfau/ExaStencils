package exastencils.field.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir._
import exastencils.deprecated.ir._
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.grid.ir._
import exastencils.util.ir.IR_Read

/// IR_ReadField

object IR_ReadField {
  private var counter : Int = 0
  def getNewName() : String = {
    counter += 1
    "fieldReadStream_%02d".format(counter)
  }
}

case class IR_ReadField(var filename : IR_Expression, var field : IR_FieldSelection, var condition : IR_Expression = true, var includeGhostLayers : Boolean = false) extends IR_Statement with IR_Expandable {
  def numDimsGrid = field.fieldLayout.numDimsGrid
  def numDimsData = field.fieldLayout.numDimsData

  def getPos(field : IR_FieldSelection, dim : Int) : IR_Expression = {
    // TODO: add function to field (layout) to decide node/cell for given dim
    field.field.localization match {
      case IR_AtNode              => IR_VF_NodePositionPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
      case IR_AtCellCenter        => IR_VF_CellCenterPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
      case IR_AtFaceCenter(`dim`) => IR_VF_NodePositionPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
      case IR_AtFaceCenter(_)     => IR_VF_CellCenterPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
    }
  }

  override def expand() : Output[StatementList] = {
    if (!Settings.additionalIncludes.contains("fstream"))
      Settings.additionalIncludes += "fstream"

    // TODO: incorporate component accesses
    val arrayIndexRange = 0 until field.field.gridDatatype.resolveFlattendSize

    val streamName = IR_ReadField.getNewName()
    def streamType = IR_SpecialDatatype("std::ifstream")
    def stream = IR_VariableAccess(streamName, streamType)

    val read = IR_Read(stream)
    arrayIndexRange.foreach { index =>
      val access = IR_FieldAccess(field, IR_LoopOverDimensions.defIt(numDimsData))
      if (numDimsData > numDimsGrid) // TODO: replace after implementing new field accessors
        access.index(numDimsData - 1) = index // TODO: assumes innermost dimension to represent vector index
      read.toRead += access
    }

    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements += IR_ObjectInstantiation(stream, Duplicate(filename))

    def beginId = if (includeGhostLayers) "GLB" else "DLB"
    def endId = if (includeGhostLayers) "GRE" else "DRE"

    statements +=
      IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(field.domainIndex),
          IR_LoopOverDimensions(numDimsData, IR_ExpressionIndexRange(
            IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.fieldLayout.idxById(beginId, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression)),
            IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.fieldLayout.idxById(endId, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression))),
            IR_IfCondition(condition, read))))

    statements += IR_MemberFunctionCall(stream, "close")

    statements
  }
}
