package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.domain.ir
import exastencils.field.ir._
import exastencils.logger.Logger

case class IR_FileAccess_MPIIO(
    var filename : IR_Expression,
    var field : IR_Field,
    var slot : IR_Expression,
    var includeGhostLayers : Boolean, // TODO handling
    var writeAccess : Boolean,
    var appendedMode : Boolean = false) extends IR_FileAccess(filename, field, slot, includeGhostLayers, writeAccess, appendedMode) {

  val openMode : IR_VariableAccess = if(writeAccess) {
    val openOrCreate = if (appendedMode) "MPI_MODE_APPEND" else "MPI_MODE_CREATE"
    IR_VariableAccess("MPI_MODE_WRONLY | " + openOrCreate, IR_UnknownDatatype)
  } else {
    IR_VariableAccess("MPI_MODE_RDONLY", IR_UnknownDatatype)
  }

  // TODO: Handling collective/independent I/O

  // mpi i/o specific datatypes
  val MPI_File = IR_SpecialDatatype("MPI_File")
  val MPIIO_Datatype = IR_SpecialDatatype("MPI_Datatype")
  val mpiDatatypeField = IR_VariableAccess(field.layout.datatype.prettyprint_mpi, IR_UnknownDatatype)
  val rowMajor = IR_VariableAccess("MPI_ORDER_C", IR_UnknownDatatype)

  // declarations
  val fileHandle_decl = IR_VariableDeclaration(MPI_File, IR_FileAccess.declareVariable("fh"))
  val localView_decl = IR_VariableDeclaration(MPIIO_Datatype, IR_FileAccess.declareVariable("localSubarray"))
  val globalView_decl = IR_VariableDeclaration(IR_ArrayDatatype(MPIIO_Datatype, Knowledge.domain_numFragmentsPerBlock), IR_FileAccess.declareVariable("globalSubarray"))
  val info_decl = IR_VariableDeclaration(IR_SpecialDatatype("MPI_Info"), IR_FileAccess.declareVariable("info"), IR_VariableAccess("MPI_INFO_NULL", IR_UnknownDatatype)) //TODO handle hints
  val status_decl = IR_VariableDeclaration(IR_SpecialDatatype("MPI_Status"), IR_FileAccess.declareVariable("status"))
  val count_decl = IR_VariableDeclaration(IR_ArrayDatatype(IR_IntegerDatatype, numDimsData), IR_FileAccess.declareVariable("count"), IR_InitializerList(innerPointsLocal : _*))
  val localDims_decl = IR_VariableDeclaration(IR_ArrayDatatype(IR_IntegerDatatype, numDimsData), IR_FileAccess.declareVariable("localDims"), IR_InitializerList(totalPointsLocal : _*))
  val localStart_decl = IR_VariableDeclaration(IR_ArrayDatatype(IR_IntegerDatatype, numDimsData), IR_FileAccess.declareVariable("localStart"), IR_InitializerList(startIdxLocal : _*))
  val globalDims_decl = IR_VariableDeclaration(IR_ArrayDatatype(IR_IntegerDatatype, numDimsData), IR_FileAccess.declareVariable("globalDims"), IR_InitializerList(innerPointsGlobal : _*))
  val globalStart_decl = IR_VariableDeclaration(IR_ArrayDatatype(IR_IntegerDatatype, numDimsData), IR_FileAccess.declareVariable("globalStart"))
  val declCollection : ListBuffer[IR_VariableDeclaration] = ListBuffer(
    fileHandle_decl, localView_decl, globalView_decl, info_decl, status_decl,
    count_decl, localDims_decl, localStart_decl, globalDims_decl, globalStart_decl
  )
  // accesses
  val fileHandle = IR_VariableAccess(fileHandle_decl)
  val globalView = IR_ArrayAccess(IR_VariableAccess(globalView_decl), IR_LoopOverFragments.defIt)
  val localView = IR_VariableAccess(localView_decl)
  val info = IR_VariableAccess(info_decl)
  val status = IR_AddressOf(IR_VariableAccess(status_decl))
  val count = IR_VariableAccess(count_decl)
  val localDims = IR_VariableAccess(localDims_decl)
  val localStart = IR_VariableAccess(localStart_decl)
  val globalDims = IR_VariableAccess(globalDims_decl)
  val globalStart = IR_VariableAccess(globalStart_decl)

  override def accessFileFragwise(accessStmts : ListBuffer[IR_Statement]) : IR_LoopOverFragments = {
    val offset = IR_IntegerConstant(0) // offset is set via "globalView" parameter
    val nativeRepresentation = IR_Cast(IR_PointerDatatype(IR_CharDatatype), IR_StringConstant("native")) // to suppress warning
    val setView : IR_Statement = IR_FunctionCall(IR_ExternalFunctionReference("MPI_File_set_view"), fileHandle, offset, mpiDatatypeField, globalView, nativeRepresentation, info)
    IR_LoopOverFragments(
      IR_IfCondition(ir.IR_IV_IsValidForDomain(field.domain.index),
         ListBuffer(setView) ++ accessStmts
      )
    )
  }

  override def createOrOpenFile() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // add decls
    declCollection.foreach(decl => statements += decl)

    // open file
    val fn = IR_Cast(IR_PointerDatatype(IR_CharDatatype), filename) // to suppress warning
    statements += IR_FunctionCall(IR_ExternalFunctionReference("MPI_File_open"), mpiCommunicator, fn, openMode, info, IR_AddressOf(fileHandle))

    statements
  }

  override def setupAccess() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // create derived datatypes
    if(Knowledge.domain_onlyRectangular) {
      // global view (location within the whole domain) per fragment
      val setOffsetFrag : ListBuffer[IR_Assignment] = numDimsDataRange.map(d => IR_Assignment(IR_ArrayAccess(globalStart, d), startIdxGlobal(d))).to[ListBuffer]
      val createGlobalSubarray : IR_Statement = IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_create_subarray"), numDimsData, globalDims, count, globalStart, rowMajor, mpiDatatypeField, IR_AddressOf(globalView))
      val commitGlobalDatatype : IR_Statement = IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_commit"), IR_AddressOf(globalView))
      statements += IR_LoopOverFragments(
        setOffsetFrag ++ ListBuffer(createGlobalSubarray) ++ ListBuffer(commitGlobalDatatype)
      )
      // local view (mainly to omit ghost layers) per fragment
      val createLocalSubarray : IR_Statement = IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_create_subarray"), numDimsData, localDims, count, localStart, rowMajor, mpiDatatypeField, IR_AddressOf(localView))
      val commitLocalDatatype : IR_Statement = IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_commit"), IR_AddressOf(localView))
      statements += createLocalSubarray
      statements += commitLocalDatatype
    } else {
      // indexed selection for blockstructured meshes: "MPI_Type_indexed"
      // TODO
    }

    statements
  }
  override def cleanupAccess() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // free derived datatypes
    if(Knowledge.domain_onlyRectangular) {
      val freeGlobalDatatype : IR_Statement = IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_free"), IR_AddressOf(globalView))
      val freeLocalDatatype : IR_Statement = IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_free"), IR_AddressOf(localView))
      statements += IR_LoopOverFragments(
        freeGlobalDatatype
      )
      statements += freeLocalDatatype
    } else {
      // TODO
    }

    statements
  }

  override def closeFile() : ListBuffer[IR_Statement] = ListBuffer(IR_FunctionCall(IR_ExternalFunctionReference("MPI_File_close"), IR_AddressOf(fileHandle)))

  override def readField() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    val numElements = IR_IntegerConstant(1) // local view contains a whole fragment (with or without fragments)
    val readCall = IR_FunctionCall(IR_ExternalFunctionReference("MPI_File_read_all"), fileHandle, fieldptr, numElements, localView, status)
    statements += accessFileFragwise(
      ListBuffer(readCall)
    )

    statements
  }

  override def writeField() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    val numElements = IR_IntegerConstant(1) // local view contains a whole fragment (with or without fragments)
    val writeCall = IR_FunctionCall(IR_ExternalFunctionReference("MPI_File_write_all"), fileHandle, fieldptr, numElements, localView, status)
    statements += accessFileFragwise(
      ListBuffer(writeCall)
    )

    statements
  }

  override def validateParams() : Unit = {
    if(!Knowledge.mpi_enabled) {
      Logger.error("MPI-I/O can only be used when MPI is enabled!")
    }
  }
}
