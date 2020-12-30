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
import exastencils.util.ir.IR_PrintBlockBinary
import exastencils.util.ir.IR_Read
import exastencils.util.ir.IR_ReadBinary
import exastencils.util.ir.IR_ReadBlockBinary

case class IR_FileAccess_FPP(
    var filename : IR_Expression,
    var dataBuffers : ListBuffer[IR_DataBuffer],
    var useBinary : Boolean,
    var writeAccess : Boolean,
    var separator : IR_Expression,
    var condition : IR_Expression,
    var optPrintComponents : Option[ListBuffer[IR_Expression]],
    var appendedMode : Boolean = false) extends IR_FileAccess("fpp", filename, dataBuffers, writeAccess, appendedMode) {

  var openFlags : String = if (writeAccess) { if (appendedMode) "std::ios::app" else "std::ios::trunc" } else "std::ios::in"
  if (useBinary)
    openFlags += " | std::ios::binary"
  override def openMode = IR_VariableAccess(openFlags, IR_UnknownDatatype)

  val bytesAccessedKnownApriori : Boolean = condition == IR_BooleanConstant(true) // if there is no condition -> required number of accessed bytes are known

  val streamName : String = IR_FieldIO.getNewStreamName()
  def streamType : IR_SpecialDatatype = if (writeAccess) IR_SpecialDatatype("std::ofstream") else IR_SpecialDatatype("std::ifstream")
  def stream = IR_VariableAccess(streamName, streamType)

  override def validateParams() : Unit = {
    separator match {
      case s : IR_StringConstant => if(s.value.length > 1) {
        Logger.error("Parameter \"separator\" has the wrong length. Should be a single character.")
      }
      case _ =>
        Logger.error("Parameter \"separator\" has wrong datatype. Should be a string constant.")
    }
  }

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

  def loopOverDims(bufIdx : Int, accessStatements : IR_Statement*) : IR_LoopOverDimensions = {
    val buffer = dataBuffers(bufIdx)
    IR_LoopOverDimensions(buffer.numDimsGrid, IR_ExpressionIndexRange(
      IR_ExpressionIndex(buffer.numDimsGridRange.map(dim => buffer.beginIndices(dim) - Duplicate(buffer.referenceOffset(dim)) : IR_Expression).toArray),
      IR_ExpressionIndex(buffer.numDimsGridRange.map(dim => buffer.endIndices(dim) - Duplicate(buffer.referenceOffset(dim)) : IR_Expression).toArray)),
      IR_IfCondition(condition, accessStatements.to[ListBuffer]))
  }

  override def accessFileFragwise(bufIdx : Int, accessStatements : ListBuffer[IR_Statement]) : IR_LoopOverFragments = {
    IR_LoopOverFragments(
      IR_IfCondition(IR_IV_IsValidForDomain(dataBuffers(bufIdx).domainIdx),
        accessStatements,
      if (writeAccess) IR_Print(stream, IR_Print.flush) else IR_NullStatement))
  }

  override def accessFileBlockwise(bufIdx : Int, accessStatements : ListBuffer[IR_Statement]) : IR_Statement = {
    IR_IfCondition(IR_IV_IsValidForDomain(dataBuffers(bufIdx).domainIdx),
      accessStatements)
  }

  override def read(bufIdx : Int) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    val buf = dataBuffers(bufIdx)

    val read = if (!useBinary) {
      // skip separator if not whitespace
      val skipSep = if (!separator.asInstanceOf[IR_StringConstant].value.trim().isEmpty) {
        // TODO: maybe implement with std::getline but will be slower
        val decl = IR_VariableDeclaration(IR_CharDatatype, IR_FileAccess.declareVariable("skipSeparator"))
        statements += decl
        Some(IR_VariableAccess(decl))
      } else {
        None
      }
      // handle accesses of hodt
      val acc = handleAccessesHodt(buf).flatMap(acc => List(acc) ++ skipSep)

      loopOverDims(bufIdx, IR_Read(stream, (if (skipSep.isDefined) acc.dropRight(1) else acc) : _*)) // cond. remove sep at end
    } else {
      IR_IfCondition(bytesAccessedKnownApriori AndAnd buf.accessWithoutExclusion,
        /* true: write whole buffer */
        IR_ReadBlockBinary(stream, buf.getBaseAddress, buf.typicalByteSizeLocal),
        /* false: write component by component in a loop */
        loopOverDims(bufIdx, IR_ReadBinary(stream, handleAccessesHodt(buf))))
    }

    statements += accessFileWithGranularity(bufIdx, ListBuffer(read))

    statements
  }

  override def write(bufIdx : Int) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    val buf = dataBuffers(bufIdx)

    val print = if (!useBinary) {
      val printComponents = optPrintComponents getOrElse ListBuffer[IR_Expression]()
      printComponents += "std::scientific"
      printComponents ++= handleAccessesHodt(buf).flatMap(acc => List(acc, separator)).dropRight(1)
      printComponents += IR_Print.newline
      loopOverDims(bufIdx, IR_Print(stream, printComponents))
    } else {
      IR_IfCondition(bytesAccessedKnownApriori AndAnd buf.accessWithoutExclusion,
        /* true: write whole buffer */
        IR_PrintBlockBinary(stream, buf.getBaseAddress, buf.typicalByteSizeLocal),
        /* false: write component by component in a loop */
        loopOverDims(bufIdx, IR_PrintBinary(stream, handleAccessesHodt(buf))))
    }

    statements += accessFileWithGranularity(bufIdx, ListBuffer(print))
  }

  override def includes : ListBuffer[String] = ListBuffer("fstream", "iomanip")
}
