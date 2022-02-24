package exastencils.visualization.ir.postprocessing.netCDF

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.OutputType
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.grid.ir._
import exastencils.io.ir._
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.postprocessing.IR_PrintVisualization

// IR_PrintExodus: Visualization interface using the ExodusII finite element data model
// does not use all features of the exodusII library, but uses it only to provide the necessary meta data for visualization
// to be used in combination with PnetCDF in order to write the data to be visualized
// to be implemented as specific printer in exastencils.application.ir

@deprecated
abstract class IR_PrintExodus() extends IR_Statement with IR_Expandable with IR_PrintVisualization {

  // to be implemented in application.ir
  def fieldnames : ListBuffer[String]
  def variableEntityType : IR_VariableAccess // currently identical for all buffers in the applications
  def elementName : String
  def nodesPerElement : Int
  def statementsForPreparation : ListBuffer[IR_Statement]
  def statementsForCleanup : ListBuffer[IR_Statement]

  def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] // contains data buffers for node positions, connectivity and field values
  def dataBuffersNodePos : ListBuffer[IR_DataBuffer]
  def dataBufferConnectivity : IR_DataBuffer
  def dataBuffersConstant : ListBuffer[IR_DataBuffer] = dataBuffersNodePos :+ dataBufferConnectivity

  def IR_IV_TimeIdxRecords() = IR_IV_TimeIndexRecordVariables(resolveId)
  def IR_IV_TimeValRecords() = IR_IV_TimeValueRecordVariables(resolveId)

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
  val truthTable_decl = IR_VariableDeclaration(IR_ArrayDatatype(IR_IntegerDatatype, numElemBlocks * numVariables), "truthTable",
    IR_InitializerList(Array.fill(numElemBlocks * numVariables)(IR_IntegerConstant(1)) : _*))

  val declarations : ListBuffer[IR_VariableDeclaration] = ListBuffer(
    exoErr_decl, exoId_decl, coordNames_decl, fieldNames_decl, wordSizeCPU_decl, wordSizeIO_decl, truthTable_decl
  )

  // accesses
  val nullptr = IR_VariableAccess("NULL", IR_UnknownDatatype)
  val info = PnetCDF_Info()
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
  def ex_create() : ListBuffer[IR_Statement] = ListBuffer(
    IR_Assignment(exoId, IR_FunctionCall(IR_ExternalFunctionReference("ex_create"), IR_FileAccess.filenameAsCString(filename), openMode, IR_AddressOf(wordSizeCPU), IR_AddressOf(wordSizeIO))))
  def ex_open() : ListBuffer[IR_Statement] = ListBuffer(
    IR_VariableDeclaration(IR_FloatDatatype, "version"),
    IR_Assignment(exoId, IR_FunctionCall(IR_ExternalFunctionReference("ex_open"), IR_FileAccess.filenameAsCString(filename), "EX_WRITE", IR_AddressOf(wordSizeCPU), IR_AddressOf(wordSizeIO), IR_AddressOf("version")))
  )
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
  def ex_put_coord(pointers : ListBuffer[IR_Expression]) : ListBuffer[IR_Statement] =
    callExodusFunction("ex_put_coord", exoId +: pointers.padTo(3, nullptr) : _*)
  def ex_put_time : ListBuffer[IR_Statement] =
    callExodusFunction("ex_put_time", exoId, IR_IV_TimeIdxRecords(), IR_AddressOf(IR_IV_TimeValRecords()))
  def ex_put_conn(ptr : IR_Expression) : ListBuffer[IR_Statement] =
    callExodusFunction("ex_put_conn", exoId, EX_ELEM_BLOCK, elemBlockId, ptr, 0, 0)
  def ex_put_var(varIndex : Int, ptr : IR_Expression) : ListBuffer[IR_Statement] = {
    val numVals = if (variableEntityType == EX_NODAL) numNodes else numElemInBlock
    callExodusFunction("ex_put_var", exoId, IR_IV_TimeIdxRecords(), variableEntityType, varIndex, elemBlockId, numVals, ptr)
  }

  def ex_close() : ListBuffer[IR_Statement] =
    callExodusFunction("ex_close", exoId)

  def ioInterface : String = "nc"

  def ioHandler(constsIncluded : Boolean, fn : IR_Expression) : IR_FileAccess_PnetCDF = {
    val appendedMode = true // we create the file via the exodus library and then open it with pnetcdf to write the data
    val recordVariables = {
      mutable.HashMap(dataBuffers(constsIncluded).zipWithIndex.map { case (buf, bufIdx) => // non-constants are record variables
        if (!Knowledge.parIO_vis_constantDataReduction)
          (bufIdx, false)
        else
          (bufIdx, !dataBuffersConstant.map(_.name).contains(buf.name))
      } : _*)
    }

    fn match {
      case sc : IR_StringConstant                                         =>
        IR_FileAccess_PnetCDF(sc, dataBuffers(constsIncluded), Some(recordVariables),
          writeAccess = true, appendedMode, initFragInfo = false, timeIdx = IR_IV_TimeIdxRecords(), timeVal = IR_IV_TimeValRecords(), altFileMode = None)
      case vAcc : IR_VariableAccess if vAcc.datatype == IR_StringDatatype =>
        if (Knowledge.parIO_vis_constantDataReduction) {
          Logger.error("Error in IR_PrintExodus: Parameter \"filename\" must be a string constant when \"Knowledge.parIO_constantDataReduction\" is enabled.")
        } else {
          IR_FileAccess_PnetCDF(vAcc, dataBuffers(constsIncluded), Some(recordVariables),
            writeAccess = true, appendedMode, initFragInfo = false, timeIdx = IR_IV_TimeIdxRecords(), timeVal = IR_IV_TimeValRecords(), altFileMode = None)
        }
      case _                                                              =>
        Logger.error("Error in IR_PrintExodus: Parameter \"filename\" has wrong datatype.")
    }
  }

  // Parallel mode: open file via exodusII library, write metadata for visualization and close
  // perform heavy I/O operations with PnetCDF
  def writeExodusMetaData() : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    // enable error messages to be printed to std::cerr
    if (Knowledge.parIO_generateDebugStatements)
      stmts += IR_FunctionCall(IR_ExternalFunctionReference("ex_opts"), IR_VariableAccess("EX_VERBOSE", IR_UnknownDatatype))

    if (Knowledge.mpi_enabled)
      stmts ++= info.setHints()

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

  // in serial applications: no mpi datatypes to describe memory layout, but exodus only creates 1D dims for variables (immutable)
  // -> unfortunately, if data is excluded (e.g. ghost layers) all data has to be copied regardless if exodusII or serial netcdf API is used
  // we directly use the serial exodusII library here for simplicity
  def writeDataSerial(constsIncluded : Boolean) : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    // enable error messages to be printed to std::cerr
    if (Knowledge.parIO_generateDebugStatements)
      stmts += IR_FunctionCall(IR_ExternalFunctionReference("ex_opts"), IR_VariableAccess("EX_VERBOSE", IR_UnknownDatatype))

    if (constsIncluded) {
      stmts += IR_Assignment(IR_IV_TimeIdxRecords(), 1) // ex_put_var expects start value 1
      stmts ++= ex_create()
      stmts ++= ex_put_init()
      stmts ++= ex_put_block()
      stmts ++= ex_put_coord_names()
      stmts ++= ex_put_variable_param()
      stmts ++= ex_put_truth_table()
      stmts ++= ex_put_variable_names()
    } else {
      stmts ++= ex_open()
    }

    stmts ++= ex_put_time

    var varIdx = 1
    dataBuffers(constsIncluded).foreach { buf =>
      val tmpBufPtr : IR_AddressOf = if (!buf.accessBlockwise && !buf.evalAccessWithoutExclusion) {
        val indexRange = IR_ExpressionIndexRange(
          IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => buf.beginIndices(dim) - Duplicate(buf.referenceOffset(dim)) : IR_Expression)),
          IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => buf.endIndices(dim) - Duplicate(buf.referenceOffset(dim)) : IR_Expression)))
        val indexRangeTransformed = buf.accessPattern.transformExpressionIndexRange(indexRange.begin, indexRange.end)

        val dims = buf.accessPattern.transformDataExtents((indexRange.end - indexRange.begin).indices.to[ListBuffer], buf.localization, orderKJI = false)
        val tmp = IR_IV_TemporaryBuffer(buf.datatype.resolveBaseDatatype, buf.localization, "tmp" + buf.name, buf.domainIdx, blockwise = true, dims)
        val accesses = buf.accessPattern.accessIndices getOrElse ListBuffer(IR_ConstIndex(Array.fill(buf.numDimsData)(0)))
        val numAccesses = buf.accessPattern.numAccesses
        val offset = IR_LoopOverFragments.defIt * dims.reduce(_ * _) + numAccesses * indexRangeTransformed.linearizeIndex(IR_LoopOverDimensions.defIt(numDimsGrid))

        stmts += tmp.allocateMemory
        stmts += IR_LoopOverFragments(
          IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
            IR_LoopOverDimensions(numDimsGrid, indexRangeTransformed,
              (0 until numAccesses).to[ListBuffer].map(idx => {
                IR_Assignment(
                  IR_IV_TemporaryBuffer.accessArray(tmp, offset + idx),
                  buf.getAccess(IR_LoopOverDimensions.defIt(numDimsGrid) + accesses(idx))) : IR_Statement
              }))))

        IR_AddressOf(IR_IV_TemporaryBuffer.accessArray(tmp, 0))
      } else {
        buf.getBaseAddress
      }

      val isCoordBuffer = dataBuffersNodePos.zipWithIndex.collectFirst { case (coordBuf, bufIdx) if coordBuf.datasetName == buf.datasetName => bufIdx }
      val isConnectivityBuffer = buf.datasetName == dataBufferConnectivity.datasetName

      if (isCoordBuffer.isDefined) {
        stmts ++= ex_put_coord(ListBuffer.fill[IR_Expression](3)(nullptr).updated(isCoordBuffer.get, tmpBufPtr))
      } else if (isConnectivityBuffer) {
        stmts ++= ex_put_conn(tmpBufPtr)
      } else {
        stmts ++= ex_put_var(varIdx, tmpBufPtr)
        varIdx += 1
      }
    }

    // update time
    if (Knowledge.parIO_vis_constantDataReduction) {
      stmts += IR_Assignment(IR_IV_TimeIdxRecords(), 1, "+=")
      stmts += IR_Assignment(IR_IV_TimeValRecords(), 1.0, "+=")
    }

    stmts ++= ex_close()

    stmts
  }

  // dims of datasets created by Exodus are flattened (contain only the "inner" points/cells/...) and require special handling
  def writeDataParallel(constsIncluded : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    val flatDimDecls : mutable.HashSet[IR_VariableDeclaration] = mutable.HashSet()
    val ioHanderNc = ioHandler(constsIncluded, filename)

    /* declare flattened dims of the datasets */
    val fragOffset = IR_IV_FragmentOffset(domainIndex) + IR_LoopOverFragments.defIt

    def declareDims(numDims : Int, name : String, localization : IR_Localization, optDims : Option[ListBuffer[IR_Expression]] = None) = {
      val dt = IR_ArrayDatatype(ioHanderNc.datatypeDimArray, numDims)
      val decl = IR_DataBuffer.declareDimensionality(dt, name, localization, optDims)
      if (flatDimDecls.add(decl)) statements += decl
      decl
    }

    // node positions: 1 spatial dim = number of nodes
    def countNodePos(numFragsPerWrite : IR_Expression) = declareDims(1, "countNodePos", IR_AtNode, Some(ListBuffer(numFragsPerWrite * numPointsPerFrag)))

    lazy val startNodePos = declareDims(1, "startNodePos", IR_AtNode) -> ListBuffer[IR_Expression](fragOffset * numPointsPerFrag)

    // connectivity: 2 spatial dims = (number of elements, nodes per element)
    def countConnectivity(numFragsPerWrite : IR_Expression) = declareDims(2, "countConnectivity", IR_AtCellCenter, Some(ListBuffer(numFragsPerWrite * numCellsPerFrag, nodesPerElement)))

    lazy val startConnectivity = declareDims(2, "startConnectivity", IR_AtCellCenter) -> ListBuffer[IR_Expression](fragOffset * numCellsPerFrag, 0)

    // set global start index for each process, handle "invalid" fragments and write data
    def writeExodusDatasets() : mutable.ListBuffer[IR_Statement] = {
      // write data
      dataBuffers(constsIncluded).zipWithIndex.map { case (buf, bufIdx) =>
        // declare extents of variables: 1 spatial dim and 1 temporal dim: (timestep, number of field values [dep. on localization])
        val spatialDimField : IR_Expression = variableEntityType match {
          case EX_NODAL      => numPointsPerFrag
          case EX_ELEM_BLOCK => numCellsPerFrag
          case _             => Logger.error("Unknown entity type used for field data in \"IR_PrintExodus\":" + variableEntityType.name)
        }

        def countFieldData(numFragsPerWrite : IR_Expression) = declareDims(2, "countFieldData", buf.localization, Some(ListBuffer(1, numFragsPerWrite * spatialDimField)))

        val startFieldData = declareDims(2, "startFieldData", buf.localization) -> ListBuffer[IR_Expression](IR_IV_TimeIdxRecords(), fragOffset * spatialDimField)

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
        val emptyCount = IR_VariableAccess("emptyCount", IR_ArrayDatatype(ioHanderNc.MPI_Offset, numDims))
        val countSelection = IR_VariableDeclaration(IR_PointerDatatype(ioHanderNc.MPI_Offset), IR_FileAccess.declareVariable("countSelection"), IR_VariableAccess(count))
        val localView = ioHanderNc.localView(bufIdx)

        // inner statement block of a fragment/block loop
        var writeStatements : ListBuffer[IR_Statement] = ListBuffer()
        if (Knowledge.parIO_useCollectiveIO) {
          // collective calls: NOP-write for "invalid" fragments
          writeStatements += IR_IfCondition(IR_IV_IsValidForDomain(buf.domainIdx),
            start._2.zipWithIndex.map { case (extent, dim) => IR_Assignment(IR_ArrayAccess(IR_VariableAccess(start._1), dim), extent) : IR_Statement })
          writeStatements += IR_VariableDeclaration(emptyCount, new IR_InitializerList(ListBuffer.fill(numDims)(0)))
          writeStatements ++= ioHanderNc.condAssignCount(bufIdx, Some(countSelection), Some(emptyCount))

          writeStatements ++= ioHanderNc.ncmpi_put_vara_all(
            ioHanderNc.ncFile, ioHanderNc.varIdBuffer(bufIdx), IR_VariableAccess(start._1), IR_VariableAccess(countSelection), buf.getBaseAddress, ioHanderNc.bufcount, localView.getAccess
          )
        } else {
          // independent calls
          writeStatements ++= start._2.zipWithIndex.map { case (extent, dim) => IR_Assignment(IR_ArrayAccess(IR_VariableAccess(start._1), dim), extent) : IR_Statement }
          writeStatements ++= ioHanderNc.ncmpi_put_vara(
            ioHanderNc.ncFile, ioHanderNc.varIdBuffer(bufIdx), IR_VariableAccess(start._1), IR_VariableAccess(count), buf.getBaseAddress, 1, localView.getAccess
          )
        }

        if (buf.accessBlockwise)
          ioHanderNc.IR_LoopOverBlocks(writeStatements)
        else
          IR_LoopOverFragments(writeStatements)
      }
    }

    // re-use most of PnetCDF interface
    statements ++= ioHanderNc.createOrOpenFile()
    statements ++= ioHanderNc.setupAccess()
    statements ++= writeExodusDatasets()
    statements ++= ioHanderNc.cleanupAccess()
    statements ++= ioHanderNc.closeFile()

    statements
  }

  // when exodus is installed with "MPI=ON" flag, the "exodusII.h" header requires "mpi.h" -> serial programs unfortunately require MPI headers and libs, too
  // alternative solution: install pure serial version additionally
  if (!Knowledge.mpi_enabled) {
    ioHandler(constsIncluded = false, filename).setTargetCompilerToMpiWrapper()
  }

  override def expand() : OutputType = {

    Logger.warn("Exodus visualization is deprecated. Please use the Vtk or Xdmf formats instead.")

    // dependencies for ExodusII library
    if (!Settings.additionalIncludes.contains("exodusII.h"))
      Settings.additionalIncludes += "exodusII.h"
    if (!Settings.additionalLibs.contains("exodus"))
      Settings.additionalLibs += "exodus"
    if (!Settings.pathsInc.contains("$(EXODUS_HOME)/include"))
      Settings.pathsInc += "$(EXODUS_HOME)/include"
    if (!Settings.pathsLib.contains("$(EXODUS_HOME)/lib"))
      Settings.pathsLib += "$(EXODUS_HOME)/lib"

    // check if cell field conforms grid dimensions
    conformsGridDimensions(someCellField)

    if (Knowledge.parIO_vis_constantDataReduction) {
      filename match {
        case _ : IR_StringConstant => Logger.warn("Constants are reduced but filename is constant; Do not use \"printField\" in a loop with this parameter combination, otherwise the reduction will go wrong.")
        case _                     =>
      }
    }

    // dependencies for I/O interface
    ioHandler(constsIncluded = false, filename).handleDependencies()

    // preparations
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    statements ++= statementsForPreparation

    // declarations for exodus
    statements ++= declarations
    if (Knowledge.mpi_enabled)
      statements += IR_IfCondition(IR_ConstantsWrittenToFile().isEmpty, writeExodusMetaData())

    // free buffer if only used once, others are used in each print step and free'd later
    val freeTmpBuffersConst : ListBuffer[IR_Statement] = ListBuffer()
    dataBuffersConstant.foreach(constBuf => {
      if (Knowledge.parIO_vis_constantDataReduction && constBuf.isTemporaryBuffer) {
        if (constBuf.accessBlockwise) {
          freeTmpBuffersConst += IR_IfCondition(constBuf.name,
            ListBuffer[IR_Statement](
              IR_ArrayFree(constBuf.name),
              IR_Assignment(constBuf.name, 0)))
        } else {
          Logger.error("Unimplemented: temp. buffers are currently only stored block-wise")
        }
      }
    })

    // write data via exodusII (serial) or PnetCDF (parallel) interface
    statements += IR_IfCondition(IR_ConstantsWrittenToFile().isEmpty,
      /* true: write constants and field data to file and mark that constants were already written */
      (if (Knowledge.mpi_enabled) writeDataParallel(constsIncluded = true) else writeDataSerial(constsIncluded = true))
        ++ freeTmpBuffersConst
        :+ IR_ConstantsWrittenToFile().setFilename(filename),
      /* false: only write field data */
      if (Knowledge.mpi_enabled) writeDataParallel(constsIncluded = false) else writeDataSerial(constsIncluded = false))

    statements
  }
}
