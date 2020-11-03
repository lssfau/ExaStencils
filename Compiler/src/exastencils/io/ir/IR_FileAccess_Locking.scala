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
import exastencils.domain.ir._
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
    var condition : Option[IR_Expression]) extends IR_FileAccess(filename, field, slot, includeGhostLayers, writeAccess) {

  val arrayIndexRange = 0 until field.gridDatatype.resolveFlattendSize

  def separator = IR_StringConstant(if (!useAscii) "" else if (Knowledge.experimental_generateParaviewFiles) "," else " ")
  var openMode = if (Knowledge.mpi_enabled) "std::ios::app" else "std::ios::trunc" // TODO appended mode for multiple fields in file
  if (!useAscii)
    openMode += " | std::ios::binary"

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

    val read = IR_Read(stream)
    //    arrayIndexRange.foreach { index =>
    val access = IR_FieldAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDimsData))
    //      if (numDimsData > numDimsGrid) // TODO: replace after implementing new field accessors
    //        access.index(numDimsData - 2) = index // TODO: other hodt
    read.toRead += access
    if(separator.value != " ") { // skip separator
      // TODO: maybe implement with std::getline
      val decl = IR_VariableDeclaration(IR_CharDatatype, "skipSeparator")
      statements += decl
      read.toRead += IR_VariableAccess(decl)
    }

    var innerLoop = ListBuffer[IR_Statement](
      IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess(openMode, IR_UnknownDatatype)),
      IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(field.domain.index),
          IR_LoopOverDimensions(numDimsData, IR_ExpressionIndexRange(
            IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.layout.idxById(beginId, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression)),
            IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.layout.idxById(endId, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression))),
            IR_IfCondition(condition.getOrElse(IR_BooleanConstant(true)), read))))
      ,
      IR_MemberFunctionCall(stream, "close")
    )

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

    // TODO: less monolithic code
    var innerLoop = ListBuffer[IR_Statement](
      IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess(openMode, IR_UnknownDatatype)),
      if (Knowledge.field_printFieldPrecision == -1)
        IR_Print(stream, "std::scientific")
      else
        IR_Print(stream, "std::scientific << std::setprecision(" + Knowledge.field_printFieldPrecision + ")"), //std::defaultfloat
      IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(field.domain.index),
          IR_LoopOverDimensions(numDimsData, IR_ExpressionIndexRange(
            IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.layout.idxById(beginId, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression)),
            IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.layout.idxById(endId, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression))),
            IR_IfCondition(condition.getOrElse(IR_BooleanConstant(true)),
              IR_Print(stream, printComponents)))),
        IR_Print(stream, IR_Print.flush)),
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