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
  def getAllViews() : ListBuffer[MPI_View] = Duplicate(globalViews) ++ Duplicate(localViews)
  def resetViews() : Unit = {
    localViews = ListBuffer()
    globalViews = ListBuffer()
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
    var appendedMode : Boolean = false) extends IR_FileAccess(filename, dataBuffers, writeAccess, appendedMode) {

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
  lazy val mpiDatatypeBuffer = (buf : IR_DataBuffer) => IR_VariableAccess(buf.datatype.resolveBaseDatatype.prettyprint_mpi, IR_UnknownDatatype)

  // declarations
  val fileHandle_decl = IR_VariableDeclaration(MPI_File, IR_FileAccess.declareVariable("fh"))
  val info_decl = IR_VariableDeclaration(IR_SpecialDatatype("MPI_Info"), IR_FileAccess.declareVariable("info"), IR_VariableAccess("MPI_INFO_NULL", IR_UnknownDatatype)) //TODO handle hints
  val status_decl = IR_VariableDeclaration(IR_SpecialDatatype("MPI_Status"), IR_FileAccess.declareVariable("status"))

  // declarations per buffer
  val count_decl = (buf: IR_DataBuffer) => IR_FileAccess.declareDimensionality(
    IR_ArrayDatatype(IR_IntegerDatatype, buf.numDimsData), "count", buf.localization, Some(buf.innerDimsLocal))
  val localDims_decl = (buf: IR_DataBuffer) => IR_FileAccess.declareDimensionality(
    IR_ArrayDatatype(IR_IntegerDatatype, buf.numDimsData), "localDims", buf.localization, Some(buf.totalDimsLocal))
  val localStart_decl = (buf: IR_DataBuffer) => IR_FileAccess.declareDimensionality(
    IR_ArrayDatatype(IR_IntegerDatatype, buf.numDimsData), "localStart", buf.localization, Some(buf.startIndexLocal))
  val globalDims_decl = (buf: IR_DataBuffer) => IR_FileAccess.declareDimensionality(
    IR_ArrayDatatype(IR_IntegerDatatype, buf.numDimsData), "globalDims", buf.localization, Some(buf.innerDimsGlobal))
  val globalStart_decl = (buf: IR_DataBuffer) => IR_FileAccess.declareDimensionality(
    IR_ArrayDatatype(IR_IntegerDatatype, buf.numDimsData), "globalStart", buf.localization)

  var declarations : ListBuffer[IR_VariableDeclaration] = ListBuffer(fileHandle_decl, info_decl, status_decl)
  for (buf <- dataBuffers) {
    declarations = (declarations :+ count_decl(buf) :+ localDims_decl(buf) :+ localStart_decl(buf) :+ globalDims_decl(buf) :+ globalStart_decl(buf)).distinct
  }

  // accesses
  val fileHandle = IR_VariableAccess(fileHandle_decl)
  val info = IR_VariableAccess(info_decl)
  val status = IR_AddressOf(IR_VariableAccess(status_decl))
  val count = (buf : IR_DataBuffer) => IR_VariableAccess(count_decl(buf))
  val localDims = (buf : IR_DataBuffer) => IR_VariableAccess(localDims_decl(buf))
  val localStart = (buf : IR_DataBuffer) => IR_VariableAccess(localStart_decl(buf))
  val globalDims = (buf : IR_DataBuffer) => IR_VariableAccess(globalDims_decl(buf))
  val globalStart = (buf : IR_DataBuffer) => IR_VariableAccess(globalStart_decl(buf))

  // derived datatypes
  val localView = (buf : IR_DataBuffer) => MPI_View.getView(dataBuffers.indexOf(buf), global = false)
  val globalView = (buf : IR_DataBuffer) => MPI_View.getView(dataBuffers.indexOf(buf), global = true)
  //val localView = (bufIdx : Int) => MPI_View.getView(bufIdx, global = false)
  //val globalView = (bufIdx : Int) => MPI_View.getView(bufIdx, global = true)

  override def accessFileFragwise(buffer : IR_DataBuffer, accessStmts : ListBuffer[IR_Statement]) : IR_LoopOverFragments = {
    val offset = IR_IntegerConstant(0) // offset is set via "globalView" parameter
    val nativeRepresentation = IR_Cast(IR_PointerDatatype(IR_CharDatatype), IR_StringConstant("native"))
    val setView : IR_Statement = IR_FunctionCall(IR_ExternalFunctionReference("MPI_File_set_view"),
      fileHandle, offset, mpiDatatypeBuffer(buffer), globalView(buffer).getAccess, nativeRepresentation, info)

    if (Knowledge.parIO_useCollectiveIO) {
      IR_LoopOverFragments(setView +: accessStmts)
    } else {
      IR_LoopOverFragments(
        setView, // setView is a collective function
        IR_IfCondition(IR_IV_IsValidForDomain(buffer.domainIdx),
          accessStmts
        )
      )
    }
  }

  override def createOrOpenFile() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // add decls
    declarations.foreach(decl => statements += decl)

    // open file
    val fn = IR_Cast(IR_PointerDatatype(IR_CharDatatype), filename) // to suppress warning
    statements += IR_FunctionCall(IR_ExternalFunctionReference("MPI_File_open"), mpiCommunicator, fn, openMode, info, IR_AddressOf(fileHandle))

    statements
  }

  override def setupAccess() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // create derived datatypes
    for (buf <- dataBuffers) {
      // global view (location within the whole domain) per fragment
      val globalView = MPI_View(globalDims(buf), count(buf), globalStart(buf),
        buf.numDimsData, buf.domainIdx, mpiDatatypeBuffer(buf), IR_ArrayDatatype(MPI_Datatype, Knowledge.domain_numFragmentsPerBlock), "globalSubarray")
      if (MPI_View.addView(dataBuffers.indexOf(buf), global = true, globalView)) {
        statements += globalView.declaration
        statements += IR_LoopOverFragments(
          IR_IfCondition(IR_IV_IsValidForDomain(buf.domainIdx), // set global start index per frag in IR_FragmentLoop
            buf.numDimsDataRange.map(d => IR_Assignment(IR_ArrayAccess(globalStart(buf), d), buf.startIndexGlobal.reverse(d)) : IR_Statement).to[ListBuffer]),
          globalView.createDatatype,
          globalView.commitDatatype
        )
      }

      // local view (mainly to omit ghost layers) per fragment
      val localView = MPI_View(localDims(buf), count(buf), localStart(buf),
        buf.numDimsData, buf.domainIdx, mpiDatatypeBuffer(buf), MPI_Datatype, "localSubarray")
      if(MPI_View.addView(dataBuffers.indexOf(buf), global = false, localView)) {
        statements += localView.declaration
        statements += localView.createDatatype
        statements += localView.commitDatatype
      }
    }

    statements
  }

  override def read(buffer : IR_DataBuffer) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    if (Knowledge.parIO_useCollectiveIO) {
      val numElements_decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("numElems"), 1)
      val condAssignNumElements = IR_IfCondition(IR_Negation(IR_IV_IsValidForDomain(buffer.domainIdx)), IR_Assignment(IR_VariableAccess(numElements_decl), 0))
      val readCall = IR_FunctionCall(IR_ExternalFunctionReference("MPI_File_read_all"),
        fileHandle, buffer.getBaseAddress, IR_VariableAccess(numElements_decl), localView(buffer).getAccess, status)

      statements += accessFileFragwise(buffer,
        ListBuffer(numElements_decl, condAssignNumElements, readCall)
      )
    } else {
      val numElements = IR_IntegerConstant(1) // derived datatype localView contains a whole fragment (with or without ghost layers)
      val readCall = IR_FunctionCall(IR_ExternalFunctionReference("MPI_File_read"),
        fileHandle, buffer.getAddress(IR_ConstIndex(0)), numElements, localView(buffer).getAccess, status)

      statements += accessFileFragwise(buffer,
        ListBuffer(readCall)
      )
    }

    statements
  }

  override def write(buffer : IR_DataBuffer) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    if (Knowledge.parIO_useCollectiveIO) {
      val numElements_decl = IR_VariableDeclaration(IR_IntegerDatatype, IR_FileAccess.declareVariable("numElems"), 1)
      val condAssignNumElements = IR_IfCondition(IR_Negation(IR_IV_IsValidForDomain(buffer.domainIdx)), IR_Assignment(IR_VariableAccess(numElements_decl), 0))
      val writeCall = IR_FunctionCall(IR_ExternalFunctionReference("MPI_File_write_all"),
        fileHandle, buffer.getBaseAddress, IR_VariableAccess(numElements_decl), localView(buffer).getAccess, status)

      statements += accessFileFragwise(buffer,
        ListBuffer(numElements_decl, condAssignNumElements, writeCall)
      )
    } else {
      val numElements = IR_IntegerConstant(1) // derived datatype localView contains a whole fragment (with or without ghost layers)
      val writeCall = IR_FunctionCall(IR_ExternalFunctionReference("MPI_File_write"),
        fileHandle, buffer.getBaseAddress, numElements, localView(buffer).getAccess, status)

      statements += accessFileFragwise(buffer,
        ListBuffer(writeCall)
      )
    }

    statements
  }

  override def cleanupAccess() : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // free derived datatypes
    for (view <- MPI_View.getAllViews()) {
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
