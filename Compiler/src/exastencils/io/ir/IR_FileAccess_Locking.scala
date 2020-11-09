package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_MemberFunctionCall
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir._
import exastencils.field.ir._
import exastencils.parallelization.api.mpi._
import exastencils.util.ir._

case class IR_FileAccess_Locking(
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

  override def prologue() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    if(writeAccess && Knowledge.mpi_enabled) {
      val streamName = IR_FieldIO.getNewStreamName()
      def streamType = IR_SpecialDatatype("std::ofstream")
      def stream = IR_VariableAccess(streamName, streamType)

      statements += IR_IfCondition(MPI_IsRootProc(),
        ListBuffer[IR_Statement](
          IR_ObjectInstantiation(streamType, streamName, Duplicate(filename), IR_VariableAccess("std::ios::trunc", IR_UnknownDatatype)),
          IR_MemberFunctionCall(stream, "close")))
    }
    statements
  }

  // file already closed by "last" process
  override def epilogue() : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)

  override def readField() : ListBuffer[IR_Statement] = {
    val streamName = IR_FieldIO.getNewStreamName()
    def streamType = IR_SpecialDatatype("std::ifstream")
    def stream = IR_VariableAccess(streamName, streamType)

    var statements : ListBuffer[IR_Statement] = ListBuffer()

    val read = if (useAscii) IR_Read(stream) else IR_ReadBinary(stream)
    //    arrayIndexRange.foreach { index =>
    val access = IR_FieldAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDimsData))
    //      if (numDimsData > numDimsGrid) // TODO: replace after implementing new field accessors
    //        access.index(numDimsData - 2) = index // TODO: other hodt
    read.exprToRead += access
    if(getSeparatorString() == ",") { // skip separator
      // TODO: maybe implement with std::getline
      val decl = IR_VariableDeclaration(IR_CharDatatype, IR_FileAccess.declareVariable("skipSeparator"))
      statements += decl
      read.exprToRead += IR_VariableAccess(decl)
    }

    val filePointerDecl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("currFilePointer"), 0)
    val filePointer = IR_VariableAccess(filePointerDecl)

    if(Knowledge.mpi_enabled)
      statements += filePointerDecl

    // open file
    var innerLoop = ListBuffer[IR_Statement]()
    innerLoop += IR_ObjectInstantiation(stream, Duplicate(filename), openMode)

    // get file pointer from previous rank
    if(Knowledge.mpi_enabled) {
      val recvRequest = IR_VariableAccess("recvRequest", "MPI_Request")

      innerLoop += IR_IfCondition(MPI_IV_MpiRank > 0, ListBuffer[IR_Statement](
        IR_VariableDeclaration(recvRequest),
        MPI_Receive(IR_AddressOf(filePointer), 1, IR_IntegerDatatype, MPI_IV_MpiRank - 1, 0, recvRequest),
        IR_FunctionCall(MPI_WaitForRequest.generateFctAccess(), IR_AddressOf(recvRequest))))
      innerLoop += IR_MemberFunctionCall(stream, "seekg", filePointer, IR_MemberAccess(stream, "beg"))
    }

    // read data from file
    innerLoop += ioStreamLoopOverFrags(stream, read, condition)

    // comm file pointer to next rank
    if (Knowledge.mpi_enabled) {
      val sendRequest = IR_VariableAccess("sendRequest", "MPI_Request")

      innerLoop += IR_Assignment(filePointer, IR_MemberFunctionCall(stream, "tellg"))
      innerLoop += IR_IfCondition(MPI_IV_MpiRank < Knowledge.mpi_numThreads - 1, ListBuffer[IR_Statement](
        IR_VariableDeclaration(sendRequest),
        MPI_Send(IR_AddressOf(filePointer), 1, IR_IntegerDatatype, MPI_IV_MpiRank + 1, 0, sendRequest),
        IR_FunctionCall(MPI_WaitForRequest.generateFctAccess(), IR_AddressOf(sendRequest))))
    }

    // close file
    innerLoop += IR_MemberFunctionCall(stream, "close")

    if (Knowledge.mpi_enabled) {
      statements += MPI_Sequential(innerLoop)
    } else {
      statements ++= innerLoop
    }

    statements
  }

  override def writeField() : ListBuffer[IR_Statement] = {
    val streamName = IR_FieldIO.getNewStreamName()
    def streamType = IR_SpecialDatatype("std::ofstream")
    def stream = IR_VariableAccess(streamName, streamType)

    val printSetPrecision = if(useAscii) {
      if (Knowledge.field_printFieldPrecision == -1)
        IR_Print(stream, "std::scientific")
      else
        IR_Print(stream, "std::scientific << std::setprecision(" + Knowledge.field_printFieldPrecision + ")") //std::defaultfloat
    } else {
      IR_NullStatement
    }

    val print = if(useAscii) {
      val printComponents = ListBuffer[IR_Expression]()
      if (!onlyValues) { // print coords for CSV files (Paraview)
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

    // TODO: less monolithic code
    var innerLoop = ListBuffer[IR_Statement](
      IR_ObjectInstantiation(stream, Duplicate(filename), openMode),
      printSetPrecision,
      ioStreamLoopOverFrags(stream, print, condition),
      IR_MemberFunctionCall(stream, "close")
    )

    var statements : ListBuffer[IR_Statement] = ListBuffer()

    if (Knowledge.mpi_enabled) {
      statements += MPI_Sequential(innerLoop)
    } else {
      statements ++= innerLoop
    }

    statements
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