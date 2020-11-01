package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expandable
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_NullStatement
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.datastructures.Transformation.OutputType
import exastencils.field.ir.IR_Field
import exastencils.grid.ir.IR_AtCellCenter
import exastencils.grid.ir.IR_AtFaceCenter
import exastencils.grid.ir.IR_AtNode
import exastencils.grid.ir.IR_VF_CellCenterPerDim
import exastencils.grid.ir.IR_VF_NodePositionPerDim

// IR_FileAccess
// Used to read/write field data from/to files

abstract class IR_FileAccess(
    filename : IR_Expression,
    field : IR_Field,
    slot : IR_Expression,
    includeGhostLayers : Boolean,
    writeAccess : Boolean) extends IR_Statement with IR_Expandable {

  def numDimsGrid = field.layout.numDimsGrid
  def numDimsData = field.layout.numDimsData

  def getFilename() : IR_Expression = filename

  def beginId = if (includeGhostLayers) "GLB" else "DLB"
  def endId = if (includeGhostLayers) "GRE" else "DRE"

  def getPos(field : IR_Field, dim : Int) : IR_Expression = {
    // TODO: add function to field (layout) to decide node/cell for given dim
    field.localization match {
      case IR_AtNode              => IR_VF_NodePositionPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
      case IR_AtCellCenter        => IR_VF_CellCenterPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
      case IR_AtFaceCenter(`dim`) => IR_VF_NodePositionPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
      case IR_AtFaceCenter(_)     => IR_VF_CellCenterPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
    }
  }

  // TODO: naming convention
  def prologue() : ListBuffer[IR_Statement]
  def kernel() : ListBuffer[IR_Statement] = {
    if(writeAccess) {
      writeField()
    } else {
      readField()
    }
  }
  def epilogue() : ListBuffer[IR_Statement]

  def readField() : ListBuffer[IR_Statement]
  def writeField() : ListBuffer[IR_Statement]
}

// basically a NullStatement. Used when wrong input arguments were passed.
case class IR_FileAccess_None(field : IR_Field, slot : IR_Expression) extends IR_FileAccess(IR_StringConstant(""), field, slot, false, false) {
  override def prologue() : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)
  override def epilogue() : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)
  override def readField() : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)
  override def writeField() : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)
  override def expand() : OutputType = IR_NullStatement
}
