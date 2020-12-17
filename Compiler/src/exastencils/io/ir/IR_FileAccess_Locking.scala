package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_MemberFunctionCall
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi._
import exastencils.util.ir._

case class IR_FileAccess_Locking(
    var filename : IR_Expression,
    var dataBuffers : ListBuffer[IR_DataBuffer],
    var useBinary : Boolean,
    var writeAccess : Boolean,
    var separator : IR_Expression,
    var condition : IR_Expression,
    var optPrintComponents : Option[ListBuffer[IR_Expression]],
    var appendedMode : Boolean = false) extends IR_FileAccess(filename, dataBuffers, writeAccess, appendedMode) {

  var openFlags : String = if (writeAccess) { if (appendedMode) "std::ios::app" else "std::ios::trunc" } else "std::ios::in"
  if (useBinary)
    openFlags += " | std::ios::binary"
  override def openMode = IR_VariableAccess(openFlags, IR_UnknownDatatype)

  val streamName : String = IR_FieldIO.getNewStreamName()
  def streamType : IR_SpecialDatatype = if (writeAccess) IR_SpecialDatatype("std::ofstream") else IR_SpecialDatatype("std::ifstream")
  def stream = IR_VariableAccess(streamName, streamType)

  override def validateParams() : Unit = {
    separator match {
      case s : IR_StringConstant => if (s.value.length > 1) {
        Logger.error("Parameter \"separator\" has the wrong length. Should be a single character.")
      }
      case _ =>
        Logger.error("Parameter \"separator\" has wrong datatype. Should be a string constant.")
    }
  }

  override def createOrOpenFile() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // for parallel programs: create file on root process before writing via locking
    if (writeAccess && Knowledge.mpi_enabled && !appendedMode) {
      val streamNameCreate = IR_FieldIO.getNewStreamName()
      def streamTypeCreate = IR_SpecialDatatype("std::ofstream")
      def streamCreate = IR_VariableAccess(streamNameCreate, streamType)

      statements += IR_IfCondition(MPI_IsRootProc(),
        ListBuffer[IR_Statement](
          IR_ObjectInstantiation(streamTypeCreate, streamNameCreate, Duplicate(filename), openMode),
          IR_MemberFunctionCall(streamCreate, "close")))
    }
    // otherwise: the file is created/opened in the MPI_Sequential accordingly

    statements
  }

  // nothing to setup here
  override def setupAccess() : ListBuffer[IR_Statement] = ListBuffer()

  override def accessFileFragwise(buffer : IR_DataBuffer, fileAcc : ListBuffer[IR_Statement]) : IR_LoopOverFragments = {
    IR_LoopOverFragments(
      IR_IfCondition(IR_IV_IsValidForDomain(buffer.domainIdx),
        IR_LoopOverDimensions(buffer.numDimsData, IR_ExpressionIndexRange(
          IR_ExpressionIndex(buffer.numDimsDataRange.map(dim => buffer.beginIndices(dim) - Duplicate(buffer.referenceOffset(dim)) : IR_Expression).toArray),
          IR_ExpressionIndex(buffer.numDimsDataRange.map(dim => buffer.endIndices(dim) - Duplicate(buffer.referenceOffset(dim)) : IR_Expression).toArray)),
          IR_IfCondition(condition, fileAcc))),
      if (writeAccess) IR_Print(stream, IR_Print.flush) else IR_NullStatement)
  }

  // nothing to cleanup & file already closed by "last" process
  override def cleanupAccess() : ListBuffer[IR_Statement] = ListBuffer()
  override def closeFile() : ListBuffer[IR_Statement] = ListBuffer()

  override def read(buffer : IR_DataBuffer) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    val read = if (useBinary) IR_ReadBinary(stream) else IR_Read(stream)
    /*
    //    arrayIndexRange.foreach { index =>
    val access = IR_FieldAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDimsData))
    //      if (numDimsData > numDimsGrid) // TODO: replace after implementing new field accessors
    //        access.index(numDimsData - 2) = index // TODO: other hodt
    read.exprToRead += access
    */
    // TODO
    read.exprToRead += buffer.getAccess(IR_LoopOverDimensions.defIt(buffer.numDimsData))

    // skip separator
    if (!separator.asInstanceOf[IR_StringConstant].value.trim().isEmpty) {
      // TODO: maybe implement with std::getline
      val decl = IR_VariableDeclaration(IR_CharDatatype, IR_FileAccess.declareVariable("skipSeparator"))
      statements += decl
      read.exprToRead += IR_VariableAccess(decl)
    }

    val filePointerDecl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("currFilePointer"), 0)
    val filePointer = IR_VariableAccess(filePointerDecl)

    if (Knowledge.mpi_enabled)
      statements += filePointerDecl

    // open file
    var innerLoop = ListBuffer[IR_Statement]()
    innerLoop += IR_ObjectInstantiation(stream, Duplicate(filename), openMode)

    // get file pointer from previous rank
    if (Knowledge.mpi_enabled) {
      val recvRequest = IR_VariableAccess("recvRequest", "MPI_Request")

      innerLoop += IR_IfCondition(MPI_IV_MpiRank > 0, ListBuffer[IR_Statement](
        IR_VariableDeclaration(recvRequest),
        MPI_Receive(IR_AddressOf(filePointer), 1, IR_IntegerDatatype, MPI_IV_MpiRank - 1, 0, recvRequest),
        IR_FunctionCall(MPI_WaitForRequest.generateFctAccess(), IR_AddressOf(recvRequest))))
      innerLoop += IR_MemberFunctionCall(stream, "seekg", filePointer, IR_MemberAccess(stream, "beg"))
    }

    // read data from file
    innerLoop += accessFileFragwise(buffer, ListBuffer(read))

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

  override def write(buffer : IR_DataBuffer) : ListBuffer[IR_Statement] = {
    val printSetPrecision = if (!useBinary) {
      if (Knowledge.field_printFieldPrecision == -1)
        IR_Print(stream, "std::scientific")
      else
        IR_Print(stream, "std::scientific << std::setprecision(" + Knowledge.field_printFieldPrecision + ")") //std::defaultfloat
    } else {
      IR_NullStatement
    }

    val print = if (!useBinary) {
      val printComponents = optPrintComponents getOrElse ListBuffer[IR_Expression]()
      printComponents += "std::scientific"
      /*
      printComponents ++= arrayIndexRange.view.flatMap { index =>
        val access = IR_FieldAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDimsData))
        if (numDimsData > numDimsGrid) // TODO: replace after implementing new field accessors
          access.index(numDimsData - 1) = index // TODO: assumes innermost dimension to represent vector index
        List(access, separator)
      }
      */
      // TODO
      printComponents += buffer.getAccess(IR_LoopOverDimensions.defIt(buffer.numDimsData))
      printComponents += separator
      printComponents += IR_Print.newline
      IR_Print(stream, printComponents)
    } else {
      val printComponents = ListBuffer[IR_Access]()
      /*
      printComponents ++= arrayIndexRange.view.flatMap { index =>
        val access = IR_FieldAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDimsData))
        if (numDimsData > numDimsGrid) // TODO: replace after implementing new field accessors
          access.index(numDimsData - 1) = index // TODO: assumes innermost dimension to represent vector index
        List(access)
      }
      */
      // TODO
      printComponents += buffer.getAccess(IR_LoopOverDimensions.defIt(buffer.numDimsData))
      IR_PrintBinary(stream, printComponents)
    }

    // append to file in MPI_Sequential instead of truncating
    val openModeLock = if (Knowledge.mpi_enabled && !appendedMode) {
      IR_VariableAccess(openFlags.replace("std::ios::trunc", "std::ios::app"), IR_UnknownDatatype)
    } else {
      openMode
    }

    var innerLoop = ListBuffer[IR_Statement](
      IR_ObjectInstantiation(stream, Duplicate(filename), openModeLock),
      printSetPrecision,
      accessFileFragwise(buffer, ListBuffer(print)),
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

  override def includes : ListBuffer[String] = ListBuffer("fstream", "iomanip")
}