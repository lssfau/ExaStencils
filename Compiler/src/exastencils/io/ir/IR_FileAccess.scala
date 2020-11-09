package exastencils.io.ir

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.OutputType
import exastencils.domain.ir._
import exastencils.field.ir._
import exastencils.grid.ir._
import exastencils.util.ir._

// IR_FileAccess
// Used to read/write field data from/to files

object IR_FileAccess {
  // prohibit redeclaration of variables for sequences of I/O statements in the same scope
  private var declMap : HashMap[String, Int] = HashMap()
  def declareVariable(s: String) : String = {
    val counter = declMap.getOrElseUpdate(s, 0)
    declMap.update(s, counter+1)
    s + "_%02d".format(counter)
  }
}

abstract class IR_FileAccess(
    filename : IR_Expression,
    field : IR_Field,
    slot : IR_Expression,
    includeGhostLayers : Boolean,
    writeAccess : Boolean,
    appendedMode : Boolean = false) extends IR_Statement with IR_Expandable {

  def numDimsGrid = field.layout.numDimsGrid
  def numDimsData = field.layout.numDimsData

  def getFilename() : IR_Expression = filename

  def getSeparatorString() : String = {
    val ret = IR_FieldIO.getExtension(filename) match {
      case ".txt" => " "
      case ".csv" => ","
      case _ => ""
    }
    ret
  }
  def separator : IR_Expression = if(getSeparatorString() == "") IR_NullExpression else IR_StringConstant(getSeparatorString())

  def beginId = if (includeGhostLayers) "GLB" else "DLB"
  def endId = if (includeGhostLayers) "GRE" else "DRE"

  def arrayIndexRange = 0 until field.gridDatatype.resolveFlattendSize

  def ioStreamLoopOverFrags(stream : IR_VariableAccess, fileAcc : IR_Statement, condition: Option[IR_Expression]) : IR_LoopOverFragments = {
    IR_LoopOverFragments(
      IR_IfCondition(IR_IV_IsValidForDomain(field.domain.index),
        IR_LoopOverDimensions(numDimsData, IR_ExpressionIndexRange(
          IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.layout.idxById(beginId, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression)),
          IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.layout.idxById(endId, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression))),
          IR_IfCondition(condition.getOrElse(IR_BooleanConstant(true)), fileAcc))),
      if(writeAccess) IR_Print(stream, IR_Print.flush) else IR_NullStatement)
  }

  // local/global dimensions and offsets
  // TODO: handling for "includeGhostLayers" parameter
  val numDimsDataRange = (0 until numDimsData).reverse // KJI order
  def stride_local : Array[IR_Expression] = numDimsDataRange.map (_ => IR_IntegerConstant(1)).toArray
  def innerPoints_local : Array[IR_Expression] = numDimsDataRange.map(d => IR_IntegerConstant(field.layout.defIdxDupRightEnd(d) - field.layout.defIdxDupLeftBegin(d))).toArray
  def totalPoints_local : Array[IR_Expression] = numDimsDataRange.map(d => IR_IntegerConstant(field.layout.defTotal(d))).toArray
  def startIdx_local : Array[IR_Expression] = numDimsDataRange.map(d => IR_IntegerConstant(field.layout.defIdxDupLeftBegin(d))).toArray
  // TODO handling for other domains
  def innerPoints_global : Array[IR_Expression] = numDimsDataRange.map(d => Knowledge.domain_rect_numFragsTotalAsVec(d) * innerPoints_local(d)).toArray
  def startIdx_global : Array[IR_Expression] = numDimsDataRange.map(d => innerPoints_local(d) * (IR_IV_FragmentIndex(d) Mod Knowledge.domain_rect_numFragsTotalAsVec(d))).toArray

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
case class IR_FileAccess_None(field : IR_Field, slot : IR_Expression) extends IR_FileAccess(IR_StringConstant(""), field, slot, false, false, false) {
  override def prologue() : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)
  override def epilogue() : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)
  override def readField() : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)
  override def writeField() : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)
  override def expand() : OutputType = IR_NullStatement
}
