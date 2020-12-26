package exastencils.io.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.logger.Logger

/// MPI_View
object MPI_View {
  // TODO simplify?
  private var localViews : ListBuffer[MPI_View] = ListBuffer()
  private var globalViews : ListBuffer[MPI_View] = ListBuffer()
  private var lookupTableLocal : ListBuffer[Int] = ListBuffer()
  private var lookupTableGlobal : ListBuffer[Int] = ListBuffer()
  def addView(idx : Int, global : Boolean, view : MPI_View) : Boolean = {
    var ret = true
    if (global) {
      var lookup = globalViews.indexOf(view)
      if (lookup != -1) {
        ret = false
      } else {
        globalViews.append(view)
        lookup = globalViews.length-1
      }
      lookupTableGlobal += lookup
    } else {
      var lookup = localViews.indexOf(view)
      if (lookup != -1) {
        ret = false
      } else {
        localViews.append(view)
        lookup = localViews.length-1
      }
      lookupTableLocal += lookup
    }
    ret
  }
  def getView(idx : Int, global : Boolean) : MPI_View = if (global) globalViews(lookupTableGlobal(idx)) else localViews(lookupTableLocal(idx))
  def getAllViews : ListBuffer[MPI_View] = Duplicate(globalViews) ++ Duplicate(localViews)
  def resetViews() : Unit = {
    localViews = ListBuffer()
    globalViews = ListBuffer()
    lookupTableLocal = ListBuffer()
    lookupTableGlobal = ListBuffer()
  }
}

case class MPI_View(
    var totalDims : IR_VariableAccess,
    var count : IR_VariableAccess,
    var start : IR_VariableAccess,
    var numDims : Int,
    var domainIdx : Int,
    var mpiBaseDatatype : IR_VariableAccess,
    var datatype : IR_Datatype,
    var name : String) {

  val rowMajor = IR_VariableAccess("MPI_ORDER_C", IR_UnknownDatatype)
  val MPI_Datatype = IR_SpecialDatatype("MPI_Datatype")

  lazy val declName : String = IR_FileAccess.declareVariable(name)
  lazy val declaration = IR_VariableDeclaration(datatype, declName)

  // views are currently only defined per fragment (array access) or per block (variable access)
  lazy val getAccess : IR_Access = datatype match {
    case IR_ArrayDatatype(MPI_Datatype, _) =>
      IR_ArrayAccess(IR_VariableAccess(declaration), IR_LoopOverFragments.defIt)
    case MPI_Datatype =>
      IR_VariableAccess(declaration)
    case _ =>
      Logger.error("Wrong datatype passed to MPI_View: " + datatype.prettyprint)
  }

  def createDatatype : IR_Statement = IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_create_subarray"),
    numDims, totalDims, count, start, rowMajor, mpiBaseDatatype, IR_AddressOf(getAccess))
  def commitDatatype : IR_Statement = IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_commit"), IR_AddressOf(getAccess))
  def freeDatatype : IR_Statement = datatype match {
    case IR_ArrayDatatype(MPI_Datatype, _) => IR_LoopOverFragments(IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_free"), IR_AddressOf(getAccess)))
    case MPI_Datatype => IR_FunctionCall(IR_ExternalFunctionReference("MPI_Type_free"), IR_AddressOf(getAccess))
    case _ =>
      Logger.error("Wrong datatype passed to MPI_View: " + datatype.prettyprint)
  }

  // TODO: indexed selection for blockstructured meshes: "MPI_Type_indexed"
}

/// IR_FileAccess_MPIIO
case class IR_FileAccess_MPIIO(
    var filename : IR_Expression,
    var dataBuffers : ListBuffer[IR_DataBuffer],
    var writeAccess : Boolean,
    var appendedMode : Boolean = false) extends IR_FileAccess("mpiio", filename, dataBuffers, writeAccess, appendedMode) {

  override def openMode : IR_VariableAccess = if (writeAccess) {
    val openOrCreate = if (appendedMode) "MPI_MODE_APPEND" else "MPI_MODE_CREATE"
    IR_VariableAccess("MPI_MODE_WRONLY | " + openOrCreate, IR_UnknownDatatype)
  } else {
    IR_VariableAccess("MPI_MODE_RDONLY", IR_UnknownDatatype)
  }

  // TODO: Test handling collective/independent I/O for "invalid" fragments

  // mpi i/o specific datatypes
  val MPI_File = IR_SpecialDatatype("MPI_File")
  val MPI_Datatype = IR_SpecialDatatype("MPI_Datatype")

  // declarations
  val fileHandle_decl = IR_VariableDeclaration(MPI_File, IR_FileAccess.declareVariable("fh"))
  val info_decl = IR_VariableDeclaration(IR_SpecialDatatype("MPI_Info"), IR_FileAccess.declareVariable("info"), IR_VariableAccess("MPI_INFO_NULL", IR_UnknownDatatype)) //TODO handle hints
  val status_decl = IR_VariableDeclaration(IR_SpecialDatatype("MPI_Status"), IR_FileAccess.declareVariable("status"))

  var declarations : ListBuffer[IR_VariableDeclaration] = dimensionalityDeclarations :+ fileHandle_decl :+ info_decl :+ status_decl

  // accesses
  val fileHandle = IR_VariableAccess(fileHandle_decl)
  val info = IR_VariableAccess(info_decl)
  val status = IR_AddressOf(IR_VariableAccess(status_decl))

  // derived datatypes
  lazy val localView : Array[MPI_View] = dataBuffers.indices.map(bufIdx => MPI_View.getView(bufIdx, global = false)).toArray
  lazy val globalView : Array[MPI_View] = dataBuffers.indices.map(bufIdx => MPI_View.getView(bufIdx, global = true)).toArray

  override def accessFileFragwise(bufIdx : Int, accessStmts : ListBuffer[IR_Statement]) : IR_LoopOverFragments = {
    val disp = fileDisplacement(bufIdx)
    val nativeRepresentation = IR_Cast(IR_PointerDatatype(IR_CharDatatype), IR_StringConstant("native"))
    val setView : IR_Statement = IR_FunctionCall(IR_ExternalFunctionReference("MPI_File_set_view"),
      fileHandle, disp, mpiDatatypeBuffer(dataBuffers(bufIdx)), globalView(bufIdx).getAccess, nativeRepresentation, info)

    if (Knowledge.parIO_useCollectiveIO) {
      IR_LoopOverFragments(setView +: accessStmts)
    } else {
      IR_LoopOverFragments(
        setView, // setView is a collective function
        IR_IfCondition(IR_IV_IsValidForDomain(dataBuffers(bufIdx).domainIdx),
          accessStmts
        )
      )
    }
  }

  override def accessFileBlockwise(bufIdx : Int, accessStatements : ListBuffer[IR_Statement]) : IR_Statement = {
    val disp = fileDisplacement(bufIdx)
    val nativeRepresentation = IR_Cast(IR_PointerDatatype(IR_CharDatatype), IR_StringConstant("native"))
    val setView : IR_Statement = IR_FunctionCall(IR_ExternalFunctionReference("MPI_File_set_view"),
      fileHandle, disp, mpiDatatypeBuffer(dataBuffers(bufIdx)), globalView(bufIdx).getAccess, nativeRepresentation, info)

    IR_IfCondition(IR_IV_IsValidForDomain(dataBuffers(bufIdx).domainIdx),
      setView +: accessStatements)
  }

  override def createOrOpenFile() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // add decls
    declarations.foreach(decl => statements += decl)

    // open file
    val fn = filename match { // cast to suppress warning
      case sc : IR_StringConstant                                         => IR_Cast(IR_PointerDatatype(IR_CharDatatype), sc)
      case vAcc : IR_VariableAccess if vAcc.datatype == IR_StringDatatype => IR_Cast(IR_PointerDatatype(IR_CharDatatype), IR_MemberFunctionCall(vAcc, "c_str"))
      case _                                                              => Logger.error("Wrong datatype passed for parameter \"filename\" to IR_FileAccess_MPIIO. Should be a \"String\" instead of:" + _)
    }
    statements += IR_FunctionCall(IR_ExternalFunctionReference("MPI_File_open"), mpiCommunicator, fn, openMode, info, IR_AddressOf(fileHandle))

    statements
  }

  override def setupAccess() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // create derived datatypes
    for (bufIdx <- dataBuffers.indices) {
      val buf = dataBuffers(bufIdx)
      // global view (location within the whole domain) per fragment
      val datatypeGlobalView = if (buf.accessBlockwise) MPI_Datatype else IR_ArrayDatatype(MPI_Datatype, Knowledge.domain_numFragmentsPerBlock)
      val globalView = MPI_View(globalDims(bufIdx), globalCount(bufIdx), globalStart(bufIdx),
        buf.globalDimsKJI.length, buf.domainIdx, mpiDatatypeBuffer(buf), datatypeGlobalView, "globalSubarray")
      if (MPI_View.addView(bufIdx, global = true, globalView)) {
        val initDatatype = ListBuffer(
          IR_IfCondition(IR_IV_IsValidForDomain(buf.domainIdx), // set global start index
            buf.startIndexGlobalKJI.indices.map(d => IR_Assignment(IR_ArrayAccess(globalStart(bufIdx), d), buf.startIndexGlobalKJI(d)) : IR_Statement).to[ListBuffer]),
          globalView.createDatatype,
          globalView.commitDatatype)

        statements += globalView.declaration
        if (buf.accessBlockwise)
          statements += IR_ForLoop( // HACK: artificial fragment loop (of length 1) to set start indices to "startIndexGlobalKJI"
            IR_VariableDeclaration(IR_LoopOverFragments.defIt, 0), IR_Lower(IR_LoopOverFragments.defIt, 1), IR_PreIncrement(IR_LoopOverFragments.defIt), initDatatype)
        else
          statements += IR_LoopOverFragments(initDatatype)
      }

      // local view (mainly to omit ghost layers) per fragment
      val localView = MPI_View(localDims(bufIdx), localCount(bufIdx), localStart(bufIdx),
        buf.numDimsData, buf.domainIdx, mpiDatatypeBuffer(buf), MPI_Datatype, "localSubarray")
      if(MPI_View.addView(bufIdx, global = false, localView)) {
        statements += localView.declaration
        statements += localView.createDatatype
        statements += localView.commitDatatype
      }
    }

    statements
  }

  override def read(bufIdx : Int) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    if (Knowledge.parIO_useCollectiveIO) {
      val numElements_decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("numElems"), 1)
      val condAssignNumElements = IR_IfCondition(IR_Negation(IR_IV_IsValidForDomain(dataBuffers(bufIdx).domainIdx)), IR_Assignment(IR_VariableAccess(numElements_decl), 0))
      val readCall = IR_FunctionCall(IR_ExternalFunctionReference("MPI_File_read_all"),
        fileHandle, dataBuffers(bufIdx).getBaseAddress, IR_VariableAccess(numElements_decl), localView(bufIdx).getAccess, status)

      statements += accessFileWithGranularity(bufIdx,
        ListBuffer(numElements_decl, condAssignNumElements, readCall)
      )
    } else {
      val numElements = IR_IntegerConstant(1) // derived datatype localView contains a whole fragment (with or without ghost layers)
      val readCall = IR_FunctionCall(IR_ExternalFunctionReference("MPI_File_read"),
        fileHandle, dataBuffers(bufIdx).getBaseAddress, numElements, localView(bufIdx).getAccess, status)

      statements += accessFileWithGranularity(bufIdx,
        ListBuffer(readCall)
      )
    }

    statements
  }

  override def write(bufIdx : Int) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    if (Knowledge.parIO_useCollectiveIO) {
      // determine number of derived datatypes to write
      val numElements_decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("numElems"), 1)
      val condAssignNumElements = if (!dataBuffers(bufIdx).accessBlockwise)
        IR_IfCondition(IR_Negation(IR_IV_IsValidForDomain(dataBuffers(bufIdx).domainIdx)), IR_Assignment(IR_VariableAccess(numElements_decl), 0))
      else
        IR_NullStatement

      val writeCall = IR_FunctionCall(IR_ExternalFunctionReference("MPI_File_write_all"),
        fileHandle, dataBuffers(bufIdx).getBaseAddress, IR_VariableAccess(numElements_decl), localView(bufIdx).getAccess, status)

      statements += accessFileWithGranularity(bufIdx,
        ListBuffer(numElements_decl, condAssignNumElements, writeCall)
      )
    } else {
      val numElements = IR_IntegerConstant(1) // derived datatype localView contains a whole fragment (with or without ghost layers)
      val writeCall = IR_FunctionCall(IR_ExternalFunctionReference("MPI_File_write"),
        fileHandle, dataBuffers(bufIdx).getBaseAddress, numElements, localView(bufIdx).getAccess, status)

      statements += accessFileWithGranularity(bufIdx,
        ListBuffer(writeCall)
      )
    }

    statements
  }

  override def cleanupAccess() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // free derived datatypes
    for (view <- MPI_View.getAllViews) {
      statements += view.freeDatatype
    }

    // reset map of views after everything is done
    MPI_View.resetViews()

    statements
  }

  override def closeFile() : ListBuffer[IR_Statement] = ListBuffer(IR_FunctionCall(IR_ExternalFunctionReference("MPI_File_close"), IR_AddressOf(fileHandle)))

  override def validateParams() : Unit = {
    if(!Knowledge.mpi_enabled) {
      Logger.error("MPI-I/O can only be used when MPI is enabled!")
    }
  }
}
