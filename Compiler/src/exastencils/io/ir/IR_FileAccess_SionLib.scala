package exastencils.io.ir

import scala.collection.mutable.ListBuffer
import scala.sys.process._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.config.Settings
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir._
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.util.ir.IR_Print

case class IR_FileAccess_SionLib(
    var filename : IR_Expression,
    var field : IR_Field,
    var slot : IR_Expression,
    var includeGhostLayers : Boolean,
    var writeAccess : Boolean,
    var condition: IR_Expression,
    var appendedMode : Boolean = false) extends IR_FileAccess(filename, field, slot, includeGhostLayers, writeAccess, appendedMode) {

  // datatypes
  val sion_int64 = IR_SpecialDatatype("sion_int64")
  val sion_int32 = IR_SpecialDatatype("sion_int32")
  val FILE = IR_SpecialDatatype("FILE")

  // for serial API: we assume that the number of chunks (there is always one chunk per process) in a sion file equals the number of MPI processes (1)
  val nTasks = 1
  val nPhysFiles = 1

  val bytesAccessedKnownApriori : Boolean = condition == IR_BooleanConstant(true) // if there is no condition -> required number of accessed bytes are known
  val numBytesDatatype : Int = field.layout.datatype.resolveBaseDatatype.typicalByteSize
  val numBytesFrag : IR_Multiplication = innerPointsLocal.reduce(_ * _) * numBytesDatatype
  val numBytesBlock : IR_Multiplication = numBytesFrag * Knowledge.domain_numFragmentsPerBlock // TODO only valid frags

  // declarations
  val fileId_decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("fileId"))
  val numTasks_decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("numTasks"), nTasks)
  val numPhysFiles_decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("nFiles"), nPhysFiles)
  val localCommunicator_decl = IR_VariableDeclaration(MPI_Comm, IR_FileAccess.declareVariable("lComm"), mpiCommunicator)
  val fsBlockSize_decl = IR_VariableDeclaration(sion_int32, IR_FileAccess.declareVariable("fsBlockSize"), IR_IntegerConstant(-1)) // -1: automatically determine file system block size
  val bytesAccessed_decl = IR_VariableDeclaration(IR_SpecialDatatype("size_t"), IR_FileAccess.declareVariable("bytes" + (if (writeAccess) "Written" else "Read")), 0)
  val chunkSizes_decl : IR_VariableDeclaration = if (Knowledge.mpi_enabled) {
    IR_VariableDeclaration(sion_int64, IR_FileAccess.declareVariable("chunkSize"), numBytesBlock)
  } else {
    IR_VariableDeclaration(IR_PointerDatatype(sion_int64), IR_FileAccess.declareVariable("chunkSizes"))
  }
  val globalRanks_decl : IR_VariableDeclaration = if (Knowledge.mpi_enabled) {
    IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("globalRank"), MPI_IV_MpiRank)
  } else {
    IR_VariableDeclaration(IR_PointerDatatype(IR_IntegerDatatype), IR_FileAccess.declareVariable("globalRanks"))
  }
  val filePtr_decl = IR_VariableDeclaration(IR_PointerDatatype(FILE), IR_FileAccess.declareVariable("filePtr"), nullptr)
  val newPhysFilenames_decl = IR_VariableDeclaration(IR_PointerDatatype(IR_CharDatatype), IR_FileAccess.declareVariable("newPhysFilenames"), nullptr)
  var declarations : ListBuffer[IR_VariableDeclaration] = ListBuffer(
    fileId_decl, numPhysFiles_decl, chunkSizes_decl, fsBlockSize_decl, bytesAccessed_decl, globalRanks_decl, filePtr_decl
  )

  if (Knowledge.mpi_enabled) {
    declarations += localCommunicator_decl
    declarations += newPhysFilenames_decl
  } else {
    declarations += numTasks_decl
  }

  // accesses
  val fileId = IR_VariableAccess(fileId_decl)
  val numTasks = IR_VariableAccess(numTasks_decl)
  val numPhysFiles = IR_VariableAccess(numPhysFiles_decl)
  val localCommunicator = IR_VariableAccess(localCommunicator_decl)
  val chunkSizes = IR_VariableAccess(chunkSizes_decl)
  val fsBlockSize = IR_VariableAccess(fsBlockSize_decl)
  val bytesAccessed = IR_VariableAccess(bytesAccessed_decl)
  val globalRanks = IR_VariableAccess(globalRanks_decl)
  val filePtr = IR_VariableAccess(filePtr_decl)
  val newPhysFilenames = IR_VariableAccess(newPhysFilenames_decl)

  override def openMode : IR_VariableAccess = {
    val flags = if (writeAccess) {
      if (appendedMode) "a" else "w"
    } else {
      "r"
    }
    IR_VariableAccess("\"" + flags + "\"", IR_UnknownDatatype)
  }

  override def createOrOpenFile() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // add decls
    declarations.foreach(decl => statements += decl)

    // create/open file with serial or parallel API
    if (Knowledge.mpi_enabled) {
      statements += IR_Assignment(fileId,
        IR_FunctionCall(
          IR_ExternalFunctionReference("sion_paropen_mpi"),
          filename, openMode, IR_AddressOf(numPhysFiles), mpiCommunicator, IR_AddressOf(localCommunicator),
          IR_AddressOf(chunkSizes), IR_AddressOf(fsBlockSize), IR_AddressOf(globalRanks), IR_AddressOf(filePtr), IR_AddressOf(newPhysFilenames)
        )
      )
    } else {
      // init chunksize and globalRanks
      statements += IR_ArrayAllocation(chunkSizes, sion_int64, nTasks)
      statements += IR_ArrayAllocation(globalRanks, IR_IntegerDatatype, nTasks)
      (0 until nTasks).foreach(t => statements += IR_Assignment(IR_ArrayAccess(chunkSizes, t), numBytesBlock))
      (0 until nTasks).foreach(t => statements += IR_Assignment(IR_ArrayAccess(globalRanks, t), t))

      statements += IR_Assignment(fileId,
        IR_FunctionCall(
          IR_ExternalFunctionReference("sion_open"),
          IR_Cast(IR_PointerDatatype(IR_CharDatatype), filename), openMode, IR_AddressOf(numTasks), IR_AddressOf(numPhysFiles), IR_AddressOf(chunkSizes), IR_AddressOf(fsBlockSize), IR_AddressOf(globalRanks), IR_AddressOf(filePtr)
        )
      )
    }

    statements
  }
  override def setupAccess() : ListBuffer[IR_Statement] = ListBuffer()

  override def accessFileFragwise(accessStatements : ListBuffer[IR_Statement]) : IR_LoopOverFragments = IR_LoopOverFragments(
    IR_IfCondition(
      IR_IV_IsValidForDomain(field.domain.index),
      accessStatements
    )
  )

  val checkBytesAccessed = IR_IfCondition(
    bytesAccessed Neq numBytesBlock / numBytesDatatype,
    IR_Print(
      IR_VariableAccess("std::cout", IR_UnknownDatatype),
      IR_StringConstant("Rank: "), MPI_IV_MpiRank, IR_StringConstant(". "),
      IR_VariableAccess("__FILE__", IR_UnknownDatatype), IR_StringConstant(": Error at line: "), IR_VariableAccess("__LINE__", IR_UnknownDatatype),
      IR_StringConstant(". Number of bytes read="), bytesAccessed, IR_StringConstant(" differ from : "), numBytesBlock / numBytesDatatype
    )
  )

  // read/write values from/to file and count the number of bytes that were accessed
  val bodyFragLoop : IR_Statement = if (accessWholeBuffer && bytesAccessedKnownApriori) {
    val funcName = if (writeAccess) "sion_fwrite" else "sion_fread"
    IR_Assignment(
      bytesAccessed,
      IR_FunctionCall(IR_ExternalFunctionReference(funcName), fieldptr, numBytesDatatype, numBytesFrag / numBytesDatatype, fileId),
      "+="
    )
  } else  {
    val funcName = if (writeAccess) "fwrite" else "fread" // use ANSI C function to reduce "sion_ensure_free_space" calls from "sion_fread" wrapper function
    val idxRange = IR_ExpressionIndexRange(
      IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.layout.idxById(beginId, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression)),
      IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.layout.idxById(endId, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression)))

    // write whole row of values (in x-dimension) if no condition is defined
    if (bytesAccessedKnownApriori) {
      val fieldAccess = IR_AddressOf(IR_FieldAccess(field, Duplicate(slot), IR_ExpressionIndex(IR_IntegerConstant(0) +: IR_LoopOverDimensions.defIt(numDimsData-1).indices)))
      IR_LoopOverDimensions(numDimsData-1, idxRange,
        IR_Assignment(bytesAccessed,
          IR_FunctionCall(IR_ExternalFunctionReference(funcName), fieldAccess, numBytesDatatype, innerPointsLocal(0), filePtr),
          "+="))
    } else {
      val fieldAccess = IR_AddressOf(IR_FieldAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDimsData)))
      IR_LoopOverDimensions(numDimsData, idxRange,
        IR_IfCondition(condition,
          IR_Assignment(bytesAccessed,
            IR_FunctionCall(IR_ExternalFunctionReference(funcName), fieldAccess, numBytesDatatype, 1, filePtr),
            "+=")))
    }
  }

  override def readField() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements += accessFileFragwise(ListBuffer(bodyFragLoop))

    if (Knowledge.parIO_generateDebugStatements && bytesAccessedKnownApriori) {
      statements += checkBytesAccessed
    }

    statements
  }

  override def writeField() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    val writeFrag = IR_IfCondition(
      IR_FunctionCall(IR_ExternalFunctionReference("sion_ensure_free_space"), fileId, numBytesFrag),
      bodyFragLoop
    )
    statements += accessFileFragwise(ListBuffer(writeFrag))

    if (Knowledge.parIO_generateDebugStatements && bytesAccessedKnownApriori) {
      statements += checkBytesAccessed
    }

    statements
  }

  override def cleanupAccess() : ListBuffer[IR_Statement] = if (Knowledge.mpi_enabled) {
    ListBuffer()
  }else {
    ListBuffer(
      IR_ArrayFree(chunkSizes),
      IR_ArrayFree(globalRanks)
    )
  }

  override def closeFile() : ListBuffer[IR_Statement] = {
    val funcName = if(Knowledge.mpi_enabled) "sion_parclose_mpi" else "sion_close"
    ListBuffer(IR_FunctionCall(IR_ExternalFunctionReference(funcName), fileId))
  }

  // use "sionconfig" script (comes with sionlib installation) to select correct compile flags
  val selectLibsCmd : String = "sionconfig --libs --cxx " + (if(Knowledge.mpi_enabled) "--mpi" else "--ser")
  val selectCflagsCmd : String = "sionconfig --cflags --cxx " + (if(Knowledge.mpi_enabled) "--mpi" else "--ser")
  val selectLibs : String = selectLibsCmd.!!
  val selectCflags : String = selectCflagsCmd.!!
  if(!Settings.makefile_additionalCFlags.contains(selectCflags))
    Settings.makefile_additionalCFlags += selectCflags

  override def includes : ListBuffer[String] = ListBuffer("sion.h")
  override def libraries : ListBuffer[String] = ListBuffer[String]() ++ selectLibs.split(" ").filter(f => f.startsWith("-l")).map(l => l.replace("-l", ""))
  override def pathsInc : ListBuffer[String] = super.pathsInc
  override def pathsLib : ListBuffer[String] = ListBuffer[String]() ++ selectLibs.split(" ").filter(f => f.startsWith("-L")).map(l => l.replace("-L", ""))
}
