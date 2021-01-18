package exastencils.visualization.ir

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_AddressOf
import exastencils.base.ir.IR_ArrayAccess
import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_Cast
import exastencils.base.ir.IR_CharDatatype
import exastencils.base.ir.IR_Expandable
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_InitializerList
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_PointerDatatype
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_SizeOf
import exastencils.base.ir.IR_SpecialDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_StringDatatype
import exastencils.base.ir.IR_UnknownDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.config.Settings
import exastencils.datastructures.Transformation.OutputType
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.grid.ir.IR_AtCellCenter
import exastencils.grid.ir.IR_AtNode
import exastencils.grid.ir.IR_Localization
import exastencils.io.ir.IR_DataBuffer
import exastencils.io.ir.IR_FileAccess
import exastencils.io.ir.IR_FileAccess_PnetCDF
import exastencils.io.ir.IR_IV_FragmentOffset
import exastencils.io.ir.IR_IV_NumValidFrags
import exastencils.io.ir.IR_IV_TimeIndexRecordVariables
import exastencils.io.ir.IR_MPI_View
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.util.ir.IR_Print

// IR_PrintExodus: Visualization interface using the ExodusII finite element data model
// does not use all features of the exodusII library, but uses it only to provide the necessary meta data for visualization
// to be used in combination with PnetCDF in order to write the data to be visualized
// to be implemented as specific printer in exastencils.application.ir

abstract class IR_PrintExodus() extends IR_Statement with IR_Expandable with IR_PrintVisualization {

  // to be implemented in application.ir
  def fieldnames : ListBuffer[String]
  def variableEntityType : IR_VariableAccess
  def elementName : String
  def nodesPerElement : Int
  def statementsForPreparation : ListBuffer[IR_Statement]
  def statementsForCleanup : ListBuffer[IR_Statement]
  def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] // contains data buffers for node positions, connectivity and field values

  // dataset names created by exodus
  def datasetCoords : ListBuffer[IR_StringConstant] = (0 until numDimsGrid).map(d => IR_StringConstant("coord" + ('x' + d).toChar.toString)).to[ListBuffer]
  def datasetConnectivity : IR_StringConstant = IR_StringConstant("connect" + elemBlockId)
  def datasetTime = IR_StringConstant("time_whole")
  def datasetFields : ListBuffer[IR_StringConstant] = (0 until numFields).map(field => {
    val basenameVariable = variableEntityType match {
      case EX_NODAL      => "vals_nod_var"
      case EX_ELEM_BLOCK => "vals_elem_var"
      case _             => Logger.error("Unknown variable entity type used in IR_PrintExodus.")
    }
    val elemBlockName = if (variableEntityType == EX_ELEM_BLOCK) "eb" + elemBlockId else ""
    IR_StringConstant(basenameVariable + (field + 1).toString + elemBlockName)
  }).to[ListBuffer]

  // constants
  val numNodeSets : Int = 0 // unused, allows referencing a group of nodes and specifying load/boundary cond.
  val numSideSets : Int = 0 // unused, allows specifying load/boundary cond. for the side of an element
  val numElemBlocks : Int = 1 // group of elements of the same type. no mixed topologies -> 1
  val elemBlockId : Int = 1 // id of a block of elements
  val numElem : IR_Expression = numCells // total number of elements
  val numElemInBlock : IR_Expression = numElem // number of elements per block
  val numVariables : Int = fieldnames.length

  // use "ex_put_block" instead of "ex_put_elem_block" due to deprecation warnings (options: edge, face, or element blocks)
  val elemBlockType = IR_VariableAccess("EX_ELEM_BLOCK", IR_UnknownDatatype) // use "element blocks"
  val numEdgesPerElem : Int = 0
  val numFacesPerElem : Int = 0
  val numAttrsPerElem : Int = 0

  // variable entity types (currently for SWE, NNF & NS: identical entity types for all variables)
  val EX_NODAL = IR_VariableAccess("EX_NODAL", IR_UnknownDatatype)
  val EX_ELEM_BLOCK = IR_VariableAccess("EX_ELEM_BLOCK", IR_UnknownDatatype)

  // elements start with index "1" in exodus
  override def connectivityStartIndex : Int = 1

  // declarations
  val info_decl = IR_VariableDeclaration(IR_SpecialDatatype("MPI_Info"), "info", IR_VariableAccess("MPI_INFO_NULL", IR_UnknownDatatype)) //TODO handle hints
  val exoErr_decl = IR_VariableDeclaration(IR_IntegerDatatype, "exoErr")
  val exoId_decl = IR_VariableDeclaration(IR_IntegerDatatype, "exoId")
  val coordNames_decl = IR_VariableDeclaration(IR_ArrayDatatype(IR_PointerDatatype(IR_CharDatatype), numDimsGrid), "coordNames",
    IR_InitializerList((0 until numDimsGrid).map(dim => IR_CStringConstant(('x' + dim).toChar.toString)) : _*))
  val fieldNames_decl = IR_VariableDeclaration(IR_ArrayDatatype(IR_PointerDatatype(IR_CharDatatype), numVariables), "fieldNames",
    IR_InitializerList(fieldnames.map(fn => IR_CStringConstant(fn)) : _*))
  // cpu and I/O word size of floating point data. no mixing of 4- and 8-byte numbers allowed in a single file
  val wordSizeCPU_decl = IR_VariableDeclaration(IR_IntegerDatatype, "wordSizeCPU", IR_SizeOf(IR_RealDatatype))
  val wordSizeIO_decl = IR_VariableDeclaration(IR_IntegerDatatype, "wordSizeIO", IR_SizeOf(IR_RealDatatype)) // no conversion
  // truth table indicates whether a variable is written for the elements in a block, creates netCDF variables for each enabled entry at once
  val truthTable_decl = IR_VariableDeclaration(IR_ArrayDatatype(IR_IntegerDatatype, numElemBlocks*numVariables), "truthTable",
    IR_InitializerList(Array.fill(numElemBlocks*numVariables)(IR_IntegerConstant(1)) : _*))

  val declarations : ListBuffer[IR_VariableDeclaration] = ListBuffer(
    info_decl, exoErr_decl, exoId_decl, coordNames_decl, fieldNames_decl, wordSizeCPU_decl, wordSizeIO_decl, truthTable_decl
  )

  // accesses
  val info = IR_VariableAccess(info_decl)
  val exoErr = IR_VariableAccess(exoErr_decl)
  val exoId = IR_VariableAccess(exoId_decl)
  val coordNames = IR_VariableAccess(coordNames_decl)
  val fieldNames = IR_VariableAccess(fieldNames_decl)
  val wordSizeCPU = IR_VariableAccess(wordSizeCPU_decl)
  val wordSizeIO = IR_VariableAccess(wordSizeIO_decl)
  val truthTable = IR_VariableAccess(truthTable_decl)
  val mpiCommunicator = IR_VariableAccess("mpiCommunicator", IR_UnknownDatatype)
  val openMode = IR_VariableAccess("EX_CLOBBER | EX_LARGE_MODEL", IR_UnknownDatatype)

  // helper functions
  def IR_CStringConstant(s : String) = IR_Cast(IR_PointerDatatype(IR_CharDatatype), IR_StringConstant(s))

  // debugging info for exodus library
  def callExodusFunction(funcName : String, args : IR_Expression*) : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    stmts += IR_Assignment(exoErr, IR_FunctionCall(IR_ExternalFunctionReference(funcName), args : _*))
    if (Knowledge.parIO_generateDebugStatements) {
      stmts += IR_IfCondition(exoErr Neq 0,
        ListBuffer[IR_Statement](
          IR_Print(IR_VariableAccess("std::cout", IR_UnknownDatatype),
            IR_StringConstant("Rank: "), MPI_IV_MpiRank, IR_StringConstant(". "),
            IR_VariableAccess("__FILE__", IR_UnknownDatatype), IR_StringConstant(": Error at line: "), IR_VariableAccess("__LINE__", IR_UnknownDatatype), IR_Print.endl),
          IR_FunctionCall(IR_ExternalFunctionReference("ex_err"), IR_VariableAccess("__func__", IR_UnknownDatatype), "\"\"", exoErr)
        )
      )
    }

    stmts
  }

  // library functions
  def ex_create_par() : ListBuffer[IR_Statement] = ListBuffer(
    IR_Assignment(exoId, IR_FunctionCall(IR_ExternalFunctionReference("ex_create_par"), IR_FileAccess.filenameAsCString(filename), openMode, IR_AddressOf(wordSizeCPU), IR_AddressOf(wordSizeIO), mpiCommunicator, info)))
  def ex_put_init() : ListBuffer[IR_Statement] =
    callExodusFunction("ex_put_init", exoId, IR_CStringConstant("title"), numDimsGrid, numNodes, numElem, numElemBlocks, numNodeSets, numSideSets)
  def ex_put_block() : ListBuffer[IR_Statement] =
    callExodusFunction("ex_put_block", exoId, elemBlockType, elemBlockId, IR_StringConstant(elementName), numElemInBlock, nodesPerElement, numEdgesPerElem, numFacesPerElem, numAttrsPerElem)
  def ex_put_coord_names() : ListBuffer[IR_Statement] =
    callExodusFunction("ex_put_coord_names", exoId, coordNames)
  def ex_put_variable_param() : ListBuffer[IR_Statement] =
    callExodusFunction("ex_put_variable_param", exoId, variableEntityType, numVariables)
  def ex_put_truth_table() : ListBuffer[IR_Statement] = if (variableEntityType != EX_NODAL) {
    callExodusFunction("ex_put_truth_table", exoId, variableEntityType, numElemBlocks, numVariables, truthTable)
  } else {
    ListBuffer() // this function is only meant for "element variables"
  }
  def ex_put_variable_names() : ListBuffer[IR_Statement] =
    callExodusFunction("ex_put_variable_names", exoId, variableEntityType, numVariables, IR_Cast(IR_PointerDatatype(IR_PointerDatatype(IR_CharDatatype)), fieldNames))
  def ex_close() : ListBuffer[IR_Statement] =
    callExodusFunction("ex_close", exoId)

  def ioInterface : String = "nc"

  def ioHandler(constsIncluded : Boolean, fn : IR_Expression) : IR_FileAccess_PnetCDF = {
    val appendedMode = true // we create the file via the exodus library and then open it with pnetcdf to write the data
    val recordVariables = {
      val consts = dataBuffers(true).map(_.name).diff(dataBuffers(false).map(_.name)) // extract const bufs
      mutable.HashMap(dataBuffers(constsIncluded).zipWithIndex.map { case (buf, bufIdx) => (bufIdx, !consts.contains(buf.name)) } : _*) // non-constants are record variables
    }

    fn match {
      case sc : IR_StringConstant                                         =>
        IR_FileAccess_PnetCDF(sc, dataBuffers(constsIncluded), Some(recordVariables), writeAccess = true, appendedMode)
      case vAcc : IR_VariableAccess if vAcc.datatype == IR_StringDatatype =>
        if (Knowledge.parIO_constantDataReduction) {
          Logger.error("Error in IR_PrintExodus: Parameter \"filename\" must be a string constant when \"Knowledge.parIO_constantDataReduction\" is enabled.")
        } else {
          IR_FileAccess_PnetCDF(vAcc, dataBuffers(constsIncluded), Some(recordVariables), writeAccess = true, appendedMode)
        }
      case _                                                              =>
        Logger.error("Error in IR_PrintExodus: Parameter \"filename\" has wrong datatype.")
    }
  }

  def writeExodus() : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    // TODO handling for serial applications

    // declarations for exodus
    stmts ++= declarations

    // enable error messages to be printed to std::cerr
    if (Knowledge.parIO_generateDebugStatements)
      stmts += IR_FunctionCall(IR_ExternalFunctionReference("ex_opts"), IR_VariableAccess("EX_VERBOSE", IR_UnknownDatatype))

    // open file via exodus library, write metadata for visualization and close
    stmts ++= ex_create_par()
    stmts ++= ex_put_init()
    stmts ++= ex_put_block()
    stmts ++= ex_put_coord_names()
    stmts ++= ex_put_variable_param()
    stmts ++= ex_put_truth_table()
    stmts ++= ex_put_variable_names()
    stmts ++= ex_close()

    stmts
  }

  // dims of datasets created by Exodus are flattened (contain only the "inner" points/cells/...) and require special handling
  def writeData(constsIncluded : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    val flatDimDecls : mutable.HashSet[IR_VariableDeclaration] = mutable.HashSet()
    val ncInterface = ioHandler(constsIncluded, filename)

    /* declare flattened dims of the datasets */
    val fragOffset = IR_IV_FragmentOffset(domainIndex) + IR_LoopOverFragments.defIt
    def declareDims(numDims : Int, name : String, localization : IR_Localization, optDims : Option[ListBuffer[IR_Expression]] = None) = {
      val dt = IR_ArrayDatatype(ncInterface.datatypeDimArray, numDims)
      val decl = IR_FileAccess.declareDimensionality(dt, name, localization, optDims)
      if (flatDimDecls.add(decl)) statements += decl
      decl
    }
    // node positions: 1 spatial dim = number of nodes
    def countNodePos(numFragsPerWrite : IR_Expression) = declareDims(1, "countNodePos", IR_AtNode, Some(ListBuffer(numFragsPerWrite * numPointsPerFrag)))
    lazy val startNodePos = declareDims(1, "startNodePos", IR_AtNode) -> ListBuffer[IR_Expression](fragOffset * numPointsPerFrag)
    // connectivity: 2 spatial dims = (number of elements, nodes per element)
    def countConnectivity(numFragsPerWrite : IR_Expression) = declareDims(2, "countConnectivity", IR_AtCellCenter, Some(ListBuffer(numFragsPerWrite * numCellsPerFrag, nodesPerElement)))
    lazy val startConnectivity = declareDims(2, "startConnectivity", IR_AtCellCenter) -> ListBuffer[IR_Expression](fragOffset * numCellsPerFrag, 0)

    /* create MPI Derived Datatypes to describe the memory layout of each buffer */
    // TODO free datatypes
    val localViews = dataBuffers(constsIncluded).zipWithIndex.map { case (buf, bufIdx) =>
      // declare dims for the datasets (unflattened)
      val numDims = buf.totalDimsLocalKJI.length
      val total = ncInterface.declareDimensionality("localDimsTotal", buf.localization, buf.totalDimsLocalKJI, IR_IntegerDatatype)
      val count = ncInterface.declareDimensionality("localCount", buf.localization, buf.innerDimsLocalKJI, IR_IntegerDatatype)
      val start = ncInterface.declareDimensionality("localStart", buf.localization, buf.startIndexLocalKJI, IR_IntegerDatatype)

      // only create datatype when necessary, otherwise re-use previously created datatype
      val localView = IR_MPI_View(IR_VariableAccess(total), IR_VariableAccess(count), IR_VariableAccess(start), numDims, buf.domainIdx, ncInterface.mpiDatatypeBuffer(buf), IR_SpecialDatatype("MPI_Datatype"), "localSubarray")
      if (IR_MPI_View.addView(bufIdx, global = false, localView)) {
        statements ++= ListBuffer(total, count, start)
        statements ++= ListBuffer(localView.declaration, localView.createDatatype, localView.commitDatatype)

        localView
      } else {
        IR_MPI_View.getView(bufIdx, global = false)
      }
    }
    IR_MPI_View.resetViews()

    // set global start index for each process, handle "invalid" fragments and write data
    def writeExodusDatasets() : mutable.ListBuffer[IR_Statement] = dataBuffers(constsIncluded).zipWithIndex.map { case (buf, bufIdx) =>
      // declare extents of variables: 1 spatial dim and 1 temporal dim: (timestep, number of field values [dep. on localization])
      val spatialDimField : IR_Expression = variableEntityType match {
        case EX_NODAL      => numPointsPerFrag
        case EX_ELEM_BLOCK => numCellsPerFrag
        case _             => Logger.error("Unknown entity type used for field data in \"IR_PrintExodus\":" + variableEntityType.name)
      }
      def countFieldData(numFragsPerWrite : IR_Expression) = declareDims(2, "countFieldData", buf.localization, Some(ListBuffer(1, numFragsPerWrite * spatialDimField)))
      val startFieldData = declareDims(3, "startFieldData", buf.localization) -> ListBuffer[IR_Expression](IR_IV_TimeIndexRecordVariables(), fragOffset * spatialDimField)

      // associate flattened dims with buffers depending on the buffers characteristics
      val numFragsPerWrite = if (buf.accessBlockwise) IR_IV_NumValidFrags(buf.domainIdx) else IR_IntegerConstant(1)
      val count = buf.datasetName match {
        case s : IR_StringConstant if datasetCoords.contains(s) => countNodePos(numFragsPerWrite)
        case s : IR_StringConstant if s == datasetConnectivity  => countConnectivity(numFragsPerWrite)
        case s : IR_StringConstant if datasetFields.contains(s) => countFieldData(numFragsPerWrite)
        case _                                                  => Logger.error("Unknown dataset used in IR_PrintExodus: " + buf.datasetName)
      }
      val start = buf.datasetName match {
        case s : IR_StringConstant if datasetCoords.contains(s) => startNodePos
        case s : IR_StringConstant if s == datasetConnectivity  => startConnectivity
        case s : IR_StringConstant if datasetFields.contains(s) => startFieldData
        case _                                                  => Logger.error("Unknown dataset used in IR_PrintExodus: " + buf.datasetName)
      }

      val numDims = count.datatype.asInstanceOf[IR_ArrayDatatype].numElements
      val bufCount = IR_VariableAccess(IR_FileAccess.declareVariable("bufCount"), IR_IntegerDatatype)
      val emptyCount = IR_VariableAccess("emptyCount", IR_ArrayDatatype(ncInterface.MPI_Offset, numDims))
      val countSelection = IR_VariableDeclaration(IR_PointerDatatype(ncInterface.MPI_Offset), IR_FileAccess.declareVariable("countSelection"), IR_VariableAccess(count))

      // inner statement block of a fragment/block loop
      var writeStatements : ListBuffer[IR_Statement] = ListBuffer()
      writeStatements += IR_VariableDeclaration(bufCount, 0)
      writeStatements += IR_IfCondition(IR_IV_IsValidForDomain(buf.domainIdx),
        start._2.zipWithIndex.map { case (extent, dim) => IR_Assignment(IR_ArrayAccess(IR_VariableAccess(start._1), dim), extent) : IR_Statement } :+
        IR_Assignment(bufCount, 1))
      writeStatements += IR_VariableDeclaration(emptyCount, new IR_InitializerList(ListBuffer.fill(numDims)(0)))
      writeStatements ++= ncInterface.condAssignCount(bufIdx, Some(countSelection), Some(emptyCount))
      writeStatements ++= ncInterface.ncmpi_put_vara_all(
        ncInterface.ncFile, ncInterface.varIdBuffer(bufIdx), IR_VariableAccess(start._1), IR_VariableAccess(countSelection), buf.getBaseAddress, bufCount, localViews(bufIdx).getAccess
      )

      if (buf.accessBlockwise)
        ncInterface.IR_LoopOverBlocks(writeStatements)
      else
        IR_LoopOverFragments(writeStatements)
    }

    // re-use most of PnetCDF interface
    statements ++= ncInterface.createOrOpenFile()
    statements ++= ncInterface.setupAccess()
    statements ++= writeExodusDatasets()
    statements ++= ncInterface.cleanupAccess()
    statements ++= ncInterface.closeFile()

    statements
  }

  override def expand() : OutputType = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // dependencies for ExodusII library
    if (!Settings.additionalIncludes.contains("exodusII.h"))
      Settings.additionalIncludes += "exodusII.h"
    if (!Settings.additionalLibs.contains("exodus"))
      Settings.additionalLibs += "exodus"
    if (!Settings.pathsInc.contains("$(EXODUS_HOME)/include"))
      Settings.pathsInc += "$(EXODUS_HOME)/include"
    if (!Settings.pathsLib.contains("$(EXODUS_HOME)/lib"))
      Settings.pathsLib += "$(EXODUS_HOME)/lib"

    // dependencies for I/O interface
    ioHandler(constsIncluded = false, filename).handleDependencies()

    // preparations
    statements ++= statementsForPreparation

    // enable visualization via exodusII
    statements += IR_IfCondition(IR_IV_ConstantsWrittenToFile().isEmpty,
      writeExodus())

    // write data via PnetCDF interface
    statements += IR_IfCondition(IR_IV_ConstantsWrittenToFile().isEmpty,
      /* true: write constants and field data to file and mark that constants were already written */
      writeData(constsIncluded = true) :+ IR_IV_ConstantsWrittenToFile().setFilename(filename),
      /* false: only write field data */
      writeData(constsIncluded = false))

    statements
  }
}
