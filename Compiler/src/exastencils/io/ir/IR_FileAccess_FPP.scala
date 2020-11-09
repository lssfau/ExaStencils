package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir._
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
    var useAscii : Boolean,
    var writeAccess : Boolean,
    var onlyValues : Boolean = true,
    var appendedMode : Boolean = false,
    var condition : Option[IR_Expression]) extends IR_FileAccess(filename, field, slot, includeGhostLayers, writeAccess, appendedMode) {

  var openFlags = if (writeAccess) { if (Knowledge.mpi_enabled || appendedMode) "std::ios::app" else "std::ios::trunc"} else "std::ios::in"
  if (!useAscii)
    openFlags += " | std::ios::binary"
  val openMode = IR_VariableAccess(openFlags, IR_UnknownDatatype)

  val fctName = if (writeAccess) {
    if (onlyValues) "WriteField" else "PrintField"
  } else
    "ReadField"

  val streamName = IR_FieldIO.getNewStreamName()
  def streamType = if (writeAccess) IR_SpecialDatatype("std::ofstream") else IR_SpecialDatatype("std::ifstream")
  def stream = IR_VariableAccess(streamName, streamType)

  override def prologue() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    filename match {
      case filenameStrConst : IR_StringConstant =>
        val str : String = filenameStrConst.value
        val strSplit = ListBuffer(str.split("\\$blockId") : _ *)
        if(!str.contains("$blockId")) {
          Logger.error("Error in \"" + fctName +"\" using file-per-process: Parameter \"basename\" must contain substring: \"$blockId\".")
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
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    val read = if (useAscii) IR_Read(stream) else IR_ReadBinary(stream)
    //    arrayIndexRange.foreach { index =>
    val access = IR_FieldAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDimsData))
    //      if (numDimsData > numDimsGrid) // TODO: replace after implementing new field accessors
    //        access.index(numDimsData - 2) = index // TODO: other hodt
    read.exprToRead += access
    //    }
    if(getSeparatorString() == ",") { // skip separator
      // TODO: maybe implement with std::getline
      val decl = IR_VariableDeclaration(IR_CharDatatype, IR_FileAccess.declareVariable("skipSeparator"))
      statements += decl
      read.exprToRead += IR_VariableAccess(decl)
    }

    statements += ioStreamLoopOverFrags(stream, read, condition)

    statements
  }

  override def writeField() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    val print = if(useAscii) {
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

    statements += ioStreamLoopOverFrags(stream, print, condition)
  }

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
