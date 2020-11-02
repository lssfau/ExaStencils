package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir._
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.util.ir.IR_BuildString
import exastencils.util.ir.IR_Read

case class IR_FileAccess_FPP(
    var filename : IR_Expression,
    var field : IR_Field,
    var slot : IR_Expression,
    var includeGhostLayers : Boolean,
    var useAscii : Boolean,
    var writeAccess : Boolean,
    var onlyValues : Boolean = true,
    var condition : Option[IR_Expression]) extends IR_FileAccess(filename, field, slot, includeGhostLayers, writeAccess) {

  val arrayIndexRange = 0 until field.gridDatatype.resolveFlattendSize

  def fctName = if (writeAccess) {
    if (onlyValues) "WriteField" else "PrintField"
  } else
    "ReadField"

  val streamName = IR_FieldIO.getNewStreamName()
  def streamType = IR_SpecialDatatype("std::ifstream") // TODO: distinguish read/write
  def stream = IR_VariableAccess(streamName, streamType)

  override def prologue() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    filename match {
      case filenameStrConst : IR_StringConstant =>
        val str : String = filenameStrConst.value
        val strSplit = ListBuffer(str.split("\\$blockId") : _ *)
        if(!str.contains("$blockId")) {
          Logger.error("Error in \"" + fctName +"\" using file-per-process: Parameter \"basename\" must contain sequence: \"$blockId\".")
        }

        val strListMpi = strSplit.flatMap(e => MPI_IV_MpiRank :: IR_StringConstant(e) :: Nil).tail

        val mpiFileName = IR_VariableAccess(IR_FieldIO.getNewFileName(), IR_StringDatatype)
        statements += IR_VariableDeclaration(mpiFileName)
        statements += IR_BuildString(mpiFileName, strListMpi)
        statements += IR_ObjectInstantiation(stream, Duplicate(mpiFileName))
      case _                                    =>
        statements += IR_ObjectInstantiation(stream, Duplicate(filename))
    }
  }
  override def epilogue() : ListBuffer[IR_Statement] = ListBuffer(IR_MemberFunctionCall(stream, "close"))

  override def readField() : ListBuffer[IR_Statement] = {

    val read = IR_Read(stream)
    //    arrayIndexRange.foreach { index =>
    val access = IR_FieldAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDimsData))
    //      if (numDimsData > numDimsGrid) // TODO: replace after implementing new field accessors
    //        access.index(numDimsData - 2) = index // TODO: other hodt
    read.toRead += access
    //    }

    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements +=
      IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(field.domain.index),
          IR_LoopOverDimensions(numDimsData, IR_ExpressionIndexRange(
            IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.layout.idxById(beginId, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression)),
            IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.layout.idxById(endId, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression))),
            IR_IfCondition(condition.getOrElse(IR_BooleanConstant(true)), read))))

    statements
  }

  override def writeField() : ListBuffer[IR_Statement] = ??? // TODO

  override def expand() : Output[StatementList]  = {
    if (!Settings.additionalIncludes.contains("fstream"))
      Settings.additionalIncludes += "fstream"
    if (!Settings.additionalIncludes.contains("iomanip"))
      Settings.additionalIncludes += "iomanip"

    var stmts : ListBuffer[IR_Statement] = ListBuffer()
    stmts ++= prologue()
    stmts ++= kernel()
    stmts ++= epilogue()
    stmts
  }
}
