package exastencils.field.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_AddressOf
import exastencils.base.ir.IR_ArrayAccess
import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_ForLoop
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_Lower
import exastencils.base.ir.IR_PreIncrement
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.baseExt.ir.IR_UnduplicatedVariable
import exastencils.config.Knowledge
import exastencils.config.Settings
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_FragmentId
import exastencils.domain.ir.IR_IV_FragmentIndex
import exastencils.domain.ir.IR_IV_FragmentPositionBegin
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.grid.ir.IR_AtCellCenter
import exastencils.grid.ir.IR_AtFaceCenter
import exastencils.io.ir.IR_DataBuffer
import exastencils.io.ir.IR_IV_TemporaryBuffer
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi.MPI_Gather
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.IR_PrintXdmf

// communicate fragment info to root at startup to be able to write the Xdmf file solely on root
case class IR_IV_FragmentIdPerBlock() extends IR_UnduplicatedVariable {
  def resolveAccess(curRank : IR_Expression, fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) : IR_Expression =
    IR_ArrayAccess(this, Knowledge.domain_numFragmentsPerBlock * curRank + fragmentIdx)
  override def resolveName() = s"fragmentIdOnRoot"
  override def resolveDatatype() = IR_ArrayDatatype("size_t", Knowledge.domain_numFragmentsTotal)
}

case class IR_IV_FragmentPosBeginPerBlock(var dim : Int) extends IR_UnduplicatedVariable {
  def resolveAccess(curRank : IR_Expression, fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) : IR_Expression =
    IR_ArrayAccess(this, Knowledge.domain_numFragmentsPerBlock * curRank + fragmentIdx)
  override def resolveName() = s"fragmentPosBeginOnRoot_$dim"
  override def resolveDatatype() = IR_ArrayDatatype(IR_RealDatatype, Knowledge.domain_numFragmentsTotal)
}

case class IR_IV_FragmentIndexPerBlock(var dim : Int) extends IR_UnduplicatedVariable {
  def resolveAccess(curRank : IR_Expression, fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) : IR_Expression =
    IR_ArrayAccess(this, Knowledge.domain_numFragmentsPerBlock * curRank + fragmentIdx)
  override def resolveName() = s"fragmentIndexOnRoot_$dim"
  override def resolveDatatype() = IR_ArrayDatatype(IR_IntegerDatatype, Knowledge.domain_numFragmentsTotal)
}

abstract class IR_PrintXdmfRectilinear(
    ioMethod : IR_Expression,
    binaryFpp : Boolean
) extends IR_PrintXdmf(ioMethod, binaryFpp) {

  var filename : IR_Expression
  var field : IR_Field
  var slot : IR_Expression
  var includeGhostLayers : Boolean
  var dataset : IR_Expression
  var canonicalFileLayout : Boolean
  var resolveId : Int

  /* handling staggered grids: cell-centered temp. buffers with interpolation */
  override def numCells_x : Int = (1 << level) * Knowledge.domain_fragmentLengthAsVec(0)
  override def numCells_y : Int = (1 << level) * Knowledge.domain_fragmentLengthAsVec(1)
  override def numCells_z : Int = if (numDimsGrid < 3) 1 else (1 << level) * Knowledge.domain_fragmentLengthAsVec(2)
  override def numCellsPerFrag : IR_Expression = numCells_x * numCells_y * numCells_z
  val staggerDim : Int = field.localization match {
    case IR_AtFaceCenter(dim) => dim
    case _ => -1
  }
  val tmpBufStag : Option[IR_IV_TemporaryBuffer] = if (staggerDim >= 0) {
    val dims = ListBuffer[IR_Expression](numCells_x, numCells_y, numCells_z).take(numDimsGrid)
    Some(IR_IV_TemporaryBuffer(field.resolveBaseDatatype, IR_AtCellCenter, "tmp_" + field.name, domainIndex, dims))
  } else {
    None
  }

  val dataBuffer : IR_DataBuffer = if (tmpBufStag.isEmpty) {
    // no face-centered variable -> pass field directly
    IR_DataBuffer(field, slot, includeGhostLayers, None, Some(dataset), canonicalFileLayout)
  } else {
    // face-centered variable -> pass temp. buffer with interp. values
    if (canonicalFileLayout) {
      Logger.warn("Unable to use IR_PrintXdmfField with \"canonicalOrder = true\" for face-centered variables. Flag is ignored.")
    }
    IR_DataBuffer(tmpBufStag.get, slot, None, Some(dataset))
  }

  // interpolate face centered values towards cell centers
  def interpStagField(tmpBufDest : IR_IV_TemporaryBuffer) : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()
    stmts += tmpBufDest.allocateMemory

    val indexStagDim = IR_ConstIndex(Array.fill(numDimsGrid)(0).updated(staggerDim, 1))
    def mean = 0.5 * (IR_FieldAccess(field, IR_IV_ActiveSlot(field), IR_LoopOverDimensions.defIt(numDimsGrid))
      + IR_FieldAccess(field, IR_IV_ActiveSlot(field), IR_LoopOverDimensions.defIt(numDimsGrid) + indexStagDim))

    def idxRange = IR_ExpressionIndexRange(
      IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => dataBuffer.beginIndices(dim) - dataBuffer.referenceOffset(dim) : IR_Expression)),
      IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => dataBuffer.endIndices(dim) - dataBuffer.referenceOffset(dim) : IR_Expression)))

    val linearizedIdx = idxRange.linearizeIndex(IR_LoopOverDimensions.defIt(numDimsGrid))
    stmts += IR_LoopOverFragments(
      IR_IfCondition(IR_IV_IsValidForDomain(dataBuffer.domainIdx),
        IR_LoopOverDimensions(numDimsGrid, idxRange,
          IR_Assignment(
            tmpBufDest.at(IR_LoopOverFragments.defIt * numCellsPerFrag + linearizedIdx),
            mean))))

    stmts
  }

  val dimsDt : Int = dataBuffer.numDimsData - dataBuffer.numDimsGrid

  /* gather fragment info on root */
  Settings.additionalMacros = (Settings.additionalMacros :+
    s"""#if SIZE_MAX == UCHAR_MAX
       |   #define MPI_SIZE_T MPI_UNSIGNED_CHAR
       |#elif SIZE_MAX == USHRT_MAX
       |   #define MPI_SIZE_T MPI_UNSIGNED_SHORT
       |#elif SIZE_MAX == UINT_MAX
       |   #define MPI_SIZE_T MPI_UNSIGNED
       |#elif SIZE_MAX == ULONG_MAX
       |   #define MPI_SIZE_T MPI_UNSIGNED_LONG
       |#elif SIZE_MAX == ULLONG_MAX
       |   #define MPI_SIZE_T MPI_UNSIGNED_LONG_LONG
       |#else
       |   #error "Could not determine MPI_SIZE_T"
       |#endif""".stripMargin).distinct
  protected def communicateFragIdToRoot = {
    new MPI_Gather(
      IR_AddressOf(IR_IV_FragmentId(0)),
      IR_AddressOf(IR_IV_FragmentIdPerBlock().resolveAccess(0, 0)),
      "MPI_SIZE_T",
      Knowledge.domain_numFragmentsPerBlock)
  }
  protected def communicateFragIndexToRoot = (0 until numDimsGrid).map(dim =>
    new MPI_Gather(
      IR_AddressOf(IR_IV_FragmentIndex(dim, 0)),
      IR_AddressOf(IR_IV_FragmentIndexPerBlock(dim).resolveAccess(0, 0)),
      IR_IntegerDatatype,
      Knowledge.domain_numFragmentsPerBlock))
  protected def communicateFragPosBeginToRoot = (0 until numDimsGrid).map(dim =>
    new MPI_Gather(
      IR_AddressOf(IR_IV_FragmentPositionBegin(dim, 0)),
      IR_AddressOf(IR_IV_FragmentPosBeginPerBlock(dim).resolveAccess(0, 0)),
      IR_RealDatatype,
      Knowledge.domain_numFragmentsPerBlock))

  protected def fragIdCurRank(global : Boolean, rank : IR_Expression = curRank, fragIdx : IR_Expression = IR_LoopOverFragments.defIt) : IR_Expression = {
    if (Knowledge.mpi_enabled)
      if (global) IR_IV_FragmentIdPerBlock().resolveAccess(rank, fragIdx) else fragIdx
    else
      IR_IV_FragmentId(fragIdx)
  }
  protected def fragPosBeginCurRank(dim : Int, rank : IR_Expression = curRank, fragIdx : IR_Expression = IR_LoopOverFragments.defIt) : IR_Expression = {
    if (Knowledge.mpi_enabled)
      IR_IV_FragmentPosBeginPerBlock(dim).resolveAccess(rank, fragIdx)
    else
      IR_IV_FragmentPositionBegin(dim, fragIdx)
  }
  protected def fragIndexCurRank(dim : Int, rank : IR_Expression = curRank, fragIdx : IR_Expression = IR_LoopOverFragments.defIt) : IR_Expression = {
    if (Knowledge.mpi_enabled)
      IR_IV_FragmentIndexPerBlock(dim).resolveAccess(rank, fragIdx)
    else
      IR_IV_FragmentIndex(dim, fragIdx)
  }

  // specialization for file-per-process: fields are written fragment-after-fragment (i.e. no canonical layout)
  // -> an Xdmf "Grid" element with positions must be specified for each fragment to match the layout of the written field data
  override def writeXdmfGrid(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // write grids per fragment of a single block ("XML") or of the whole domain
    val gridName = if (fmt != "XML") {
      IR_StringConstant("Grid") + IR_FunctionCall(IR_ExternalFunctionReference("std::to_string"), Knowledge.domain_numFragmentsPerBlock*curRank + IR_LoopOverFragments.defIt)
    } else {
      IR_StringConstant("Grid") + IR_FunctionCall(IR_ExternalFunctionReference("std::to_string"), IR_LoopOverFragments.defIt)
    }

    // write each subdomain with origins and spacing to the "global" file
    // this kind of loop is only used for "global" files in case of uniform meshes
    // since the handling is fundamentally different from other xdmf writers
    def loopOverRanks(body : IR_Statement*) = IR_ForLoop(
      IR_VariableDeclaration(curRank, 0),
      IR_Lower(curRank, Knowledge.mpi_numThreads),
      IR_PreIncrement(curRank),
      body.to[ListBuffer])

    val printSubdomains = IR_LoopOverFragments(
      IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
        ListBuffer[IR_Statement](
          printXdmfElement(stream, openGrid(gridName, "Uniform") : _*)) ++
          writeXdmfGeometry(stream, global) ++
          writeXdmfTopology(stream, global) ++
          writeXdmfAttributes(stream, global) :+
          printXdmfElement(stream, closeGrid)))

    if (!binaryFpp) {
      // write grids for each fragment
      statements += printXdmfElement(stream, openGrid(IR_StringConstant("Grid"), "Collection") : _*)
      if (ioInterface != "fpp")
        statements += loopOverRanks(printSubdomains)
      else
        statements += printSubdomains
      statements += printXdmfElement(stream, closeGrid)
    } else {
      statements += printSubdomains
    }

    statements
  }

  override def writeXdmfAttributes(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    val offsetDataBuffer = getSeekp(global)

    statements += printXdmfElement(stream, openAttribute(field.name, attributeType(field.gridDatatype), centeringType(dataBuffer.localization)))

    // handle non-scalar datatypes
    val joinDataItems = dimsDt > 0 && ioInterface != "fpp"
    val arrayIndexRange = dataBuffer.datatype.resolveFlattendSize
    val dimsHigherDimDatatype = if (dimsDt > 0) {
      IR_IntegerConstant(arrayIndexRange) +: dataBuffer.innerDimsPerFrag.dropRight(dimsDt) // reorder indices (e.g. for matrix: [x, y, 2, 1] -> [2, x, y])
    } else {
      dataBuffer.innerDimsPerFrag
    }
    val localDimsTotal = if (ioInterface == "fpp") {
      // in fpp schemes, all components are written at once in a IR_LoopOverDimensions
      dimsHigherDimDatatype
    } else {
      // otherwise, multidim. datatypes are output as multiple "scalar" components
      dataBuffer.innerDimsPerFrag
    }
    val dimsComponent = if (joinDataItems) {
      localDimsTotal.dropRight(dimsDt) // select a component of the datatype
    } else {
      localDimsTotal // already correctly formatted
    }

    // introduce "Function DataItem" to combine (join) components of non-scalar datatypes
    if (joinDataItems) {
      val function = "JOIN(" + (0 until arrayIndexRange).map("$"+_).mkString(",") + ")"
      statements += printXdmfElement(stream, openDataItemFunction(dimsHigherDimDatatype, function) : _*)
    }

    val numComponents = if (joinDataItems) arrayIndexRange else 1
    (0 until numComponents).foreach(component => {
      // depending on format and file layout, select the appropriate portion from the "heavy data" file for each fragment
      if ((ioInterface == "hdf5") || (ioInterface == "mpiio" && dataBuffer.canonicalOrder)) {
        // global file is hdf5 or laid out canonically -> select portion via "hyperslabs"
        val startIndexGlobal = if (dataBuffer.canonicalOrder) {
          // replace last indices (of the multidim datatype) with the component selection
          val start = dataBuffer.canonicalStartIndexGlobal((0 until field.numDimsGrid).map(dim => fragIndexCurRank(dim)))
          accessComponent(component, field.gridDatatype, start.dropRight(dimsDt)) // e.g. [global x, global y, 2, 1] -> [global x, global y, 1, 1]
        } else {
          // replace indices at the end (except "fragment dimension") with the component selection
          val start = dataBuffer.fragmentwiseStartIndexGlobal(fragIdCurRank(global = true))
          accessComponent(component, field.gridDatatype, start.dropRight(dimsDt + 1)) :+ start.last // e.g. [x, y, z, 3, 1, fragment] -> [x, y, z, 1, 1, fragment]
        }

        // conditionally add "fragment dimension (1)" to local dims
        val localDimsComponent = dimsComponent ++ ListBuffer.fill(dimsDt)(IR_IntegerConstant(1)) // extract values for one component
        val count = if (!dataBuffer.canonicalOrder) localDimsComponent :+ IR_IntegerConstant(1) else localDimsComponent
        val stride = IR_DataBuffer.handleFragmentDimension(dataBuffer, dataBuffer.stride, 1, orderKJI = false)

        statements += printXdmfElement(stream, openDataItemHyperslab(dimsComponent) : _*)
        statements += printXdmfElement(stream, dataItemHyperslabSelection(startIndexGlobal, stride, count) : _*)
        statements += printXdmfElement(stream, dataItemHyperslabSource(dataBuffer.datatype.resolveBaseDatatype, dataBuffer.globalDims, printFilename(stream, dataset), offsetDataBuffer) : _*)
      } else {
        val offsetComponent = component * dimsComponent.reduce(_ * _) * dataBuffer.datatype.resolveBaseDatatype.typicalByteSize // for non-scalar datatypes: skip to current component
        val seekp = offsetDataBuffer + offsetComponent + (if (fmt == "Binary") fragIdCurRank(global) * dataBuffer.typicalByteSizeFrag else 0) // field data laid out fragment-wise -> calc. offset to each fragment

        // file laid out fragment-wise -> select portion via offsets
        statements += printXdmfElement(stream, openDataItem(field.resolveBaseDatatype, dimsComponent, seekp) : _*)
        if (fmt == "XML") {
          val handler = ioHandler(constsIncluded = false, filenamePieceFpp)
          val printComponents = ListBuffer[IR_Expression]()
          printComponents += "std::scientific"
          printComponents += indentData
          printComponents ++= handler.getIndicesMultiDimDatatypes(dataBuffer).flatMap(idx => {
            if (dataBuffer.accessBlockwise) {
              // HACK: already within a fragment loop -> drop fragment loop from temp. buffer and access values with index from outer loop
              val accessIndices = idx.toExpressionIndex.indices.dropRight(1) :+ IR_LoopOverFragments.defIt
              List(dataBuffer.getAccess(IR_ExpressionIndex(accessIndices)), separator)
            } else {
              List(dataBuffer.getAccess(idx), separator)
            }
          }).dropRight(1) // last separator
          printComponents += IR_Print.newline

          statements += IR_LoopOverDimensions(numDimsGrid,
            IR_ExpressionIndexRange(
              IR_ExpressionIndex(dataBuffer.numDimsGridRange.map(dim => dataBuffer.beginIndices(dim) - Duplicate(dataBuffer.referenceOffset(dim)) : IR_Expression).toArray),
              IR_ExpressionIndex(dataBuffer.numDimsGridRange.map(dim => dataBuffer.endIndices(dim) - Duplicate(dataBuffer.referenceOffset(dim)) : IR_Expression).toArray)),
            IR_Print(stream, printComponents))
        } else {
          statements += printFilename(stream, dataset)
        }
      }
      statements += printXdmfElement(stream, closeDataItem)
    })

    // close data item with join function
    if (joinDataItems)
      statements += printXdmfElement(stream, closeDataItem)

    statements += printXdmfElement(stream, closeAttribute)

    statements
  }

  // VisIt needs every subdomain to be referenced explicitly whereas in ParaView it is possible to reference a process's "grid collection"
  override def refPiecesXml(stream : IR_VariableAccess) : ListBuffer[IR_Statement] = {
    if (!Knowledge.parIO_vis_generateVisItFiles) {
      super.refPiecesXml(stream)
    } else {
      (0 until Knowledge.mpi_numThreads).flatMap(r => { // ref each process's grid
        (0 until Knowledge.domain_numFragmentsPerBlock).map(f => { // if a process has a collection of grids -> ref each
          printXdmfElement(stream,
            XInclude(href = buildFilenamePiece(noPath = true, rank = IR_IntegerConstant(r)).toPrint,
              xpath = ListBuffer(IR_StringConstant("/Xdmf/Domain/Grid/Grid["), f + 1 : IR_Expression, IR_StringConstant("]")) : _*) : _*)
        })
      }).to[ListBuffer]
    }
  }
}


