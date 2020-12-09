package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.util.ir.IR_BuildString
import exastencils.util.ir.IR_Print
import exastencils.util.ir.IR_PrintBinary
import exastencils.util.ir.IR_Read
import exastencils.util.ir.IR_ReadBinary

case class IR_FileAccess_FPP(
    var filename : IR_Expression,
    var field : IR_Field,
    var slot : IR_Expression,
    var includeGhostLayers : Boolean,
    var useBinary : Boolean,
    var writeAccess : Boolean,
    var onlyValues : Boolean,
    var separator : IR_Expression,
    var condition : IR_Expression,
    var appendedMode : Boolean = false) extends IR_FileAccess(filename, field, slot, includeGhostLayers, writeAccess, appendedMode) {

  var openFlags : String = if (writeAccess) { if (appendedMode) "std::ios::app" else "std::ios::trunc" } else "std::ios::in"
  if (useBinary)
    openFlags += " | std::ios::binary"
  override def openMode = IR_VariableAccess(openFlags, IR_UnknownDatatype)

  val streamName : String = IR_FieldIO.getNewStreamName()
  def streamType : IR_SpecialDatatype = if (writeAccess) IR_SpecialDatatype("std::ofstream") else IR_SpecialDatatype("std::ifstream")
  def stream = IR_VariableAccess(streamName, streamType)

  override def createOrOpenFile() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    filename match {
      case filenameStrConst : IR_StringConstant =>
        val str : String = filenameStrConst.value
        val strSplit = ListBuffer(str.split("\\$blockId") : _ *)
        if(!str.contains("$blockId")) {
          Logger.error("Error when accessing a file with file-per-process: Parameter \"filename\" must contain substring: \"$blockId\".")
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

  // nothing to setup/cleanup
  override def setupAccess() : ListBuffer[IR_Statement] = ListBuffer()
  override def cleanupAccess() : ListBuffer[IR_Statement] = ListBuffer()

  override def closeFile() : ListBuffer[IR_Statement] = ListBuffer(IR_MemberFunctionCall(stream, "close"))

  override def accessFileFragwise(fileAcc : ListBuffer[IR_Statement]) : IR_LoopOverFragments = {
    IR_LoopOverFragments(
      IR_IfCondition(IR_IV_IsValidForDomain(field.domain.index),
        IR_LoopOverDimensions(numDimsData, IR_ExpressionIndexRange(
          IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.layout.idxById(beginId, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression)),
          IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.layout.idxById(endId, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression))),
          IR_IfCondition(condition, fileAcc))),
      if(writeAccess) IR_Print(stream, IR_Print.flush) else IR_NullStatement)
  }

  override def readField() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    val read = if (useBinary) IR_ReadBinary(stream) else IR_Read(stream)
    //    arrayIndexRange.foreach { index =>
    val access = IR_FieldAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDimsData))
    //      if (numDimsData > numDimsGrid) // TODO: replace after implementing new field accessors
    //        access.index(numDimsData - 2) = index // TODO: other hodt
    read.exprToRead += access
    //    }
    if(separator.asInstanceOf[IR_StringConstant].value == ",") { // skip separator
      // TODO: maybe implement with std::getline
      val decl = IR_VariableDeclaration(IR_CharDatatype, IR_FileAccess.declareVariable("skipSeparator"))
      statements += decl
      read.exprToRead += IR_VariableAccess(decl)
    }

    statements += accessFileFragwise(ListBuffer(read))

    statements
  }

  override def writeField() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    val print = if(!useBinary) {
      val printComponents = ListBuffer[IR_Expression]()
      if (!onlyValues) {
        printComponents += "std::defaultfloat"
        printComponents ++= (0 until numDimsGrid).view.flatMap { dim => List(getPos(field, dim), separator) }
      }
      printComponents += "std::scientific"
      printComponents ++= arrayIndexRange.view.flatMap { index =>
        val access = IR_FieldAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDimsData))
        if (numDimsData > numDimsGrid) // TODO: replace after implementing new field accessors
          access.index(numDimsData - 1) = index // TODO: assumes innermost dimension to represent vector index
        List(access, separator)
      }
      printComponents += IR_Print.newline
      IR_Print(stream, printComponents)
    } else {
      val printComponents = ListBuffer[IR_Access]()
      printComponents ++= arrayIndexRange.view.flatMap { index =>
        val access = IR_FieldAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDimsData))
        if (numDimsData > numDimsGrid) // TODO: replace after implementing new field accessors
          access.index(numDimsData - 1) = index // TODO: assumes innermost dimension to represent vector index
        List(access)
      }
      IR_PrintBinary(stream, printComponents)
    }

    statements += accessFileFragwise(ListBuffer(print))
  }

  override def includes : ListBuffer[String] = ListBuffer("fstream", "iomanip")
}
