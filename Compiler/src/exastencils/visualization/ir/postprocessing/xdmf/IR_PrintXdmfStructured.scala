package exastencils.visualization.ir.postprocessing.xdmf

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.core.StateManager
import exastencils.domain.ir._
import exastencils.field.ir._
import exastencils.grid.ir.IR_AtCellCenter
import exastencils.grid.ir.IR_AtFaceCenter
import exastencils.io.ir._
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi.MPI_Gather
import exastencils.util.ir.IR_Print

abstract class IR_PrintXdmfStructured(ioMethod : IR_Expression) extends IR_PrintXdmf(ioMethod) {

  var filename : IR_Expression
  var field : IR_Field
  var slot : IR_Expression
  var includeGhostLayers : Boolean
  var dataset : IR_Expression
  var canonicalFileLayout : Boolean
  var resolveId : Int

  // validate params
  if (includeGhostLayers) {
    Logger.error("Ghost layer visualization is currently unsupported for IR_PrintXdmfStructured!")
  }
  if (numDimsGrid < 2) {
    Logger.error("IR_PrintXdmfStructured is only usable for 2D/3D cases.")
  }
  conformsGridDimensions(field) // check if cell field conforms grid dimensions

  // append statements for preparation (i.e. gather fragment info on root) to domain function
  IR_PrintXdmfStructured.gatherFragInfoRoot(fmt)

  /* handling staggered grids: cell-centered temp. buffers with interpolation */
  override def numCells_x : Int = (1 << level) * Knowledge.domain_fragmentLengthAsVec(0)
  override def numCells_y : Int = (1 << level) * Knowledge.domain_fragmentLengthAsVec(1)
  override def numCells_z : Int = if (numDimsGrid < 3) 1 else (1 << level) * Knowledge.domain_fragmentLengthAsVec(2)
  override def numCellsPerFrag : IR_Expression = numCells_x * numCells_y * numCells_z
  val staggerDim : Int = field.localization match {
    case IR_AtFaceCenter(dim) => dim
    case _                    => -1
  }
  val tmpBufStag : Option[IR_IV_TemporaryBuffer] = if (staggerDim >= 0) {
    val dims = ListBuffer[IR_Expression](numCells_x, numCells_y, numCells_z).take(numDimsGrid)
    Some(IR_IV_TemporaryBuffer(field.resolveBaseDatatype, IR_AtCellCenter, IR_FileAccess.declareVariable(s"tmp_${field.codeName}"), domainIndex, blockwise = !canonicalFileLayout, dims))
  } else {
    None
  }

  val dataBuffer : IR_DataBuffer = if (tmpBufStag.isEmpty) {
    // no face-centered variable -> pass field directly
    IR_DataBuffer(field, slot, includeGhostLayers, None, Some(dataset), canonicalFileLayout)
  } else {
    // face-centered variable -> pass temp. buffer with interp. values
    IR_DataBuffer(tmpBufStag.get, slot, None, Some(dataset), canonicalFileLayout)
  }

  override def stmtsForPreparation : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements ++= IR_IV_FragmentInfo.init(
      dataBuffer.domainIdx,
      calculateFragOffset = ioInterface != "fpp"
    )

    val varname = IR_FileAccess.declareVariable("fragOffset")
    statements += IR_VariableDeclaration(IR_IntegerDatatype, varname)
    statements += loopOverRanks(
      IR_IfCondition(curRank > 0,
        IR_Assignment(varname, IR_IV_NumValidFragsPerBlock(domainIndex).resolveAccess(curRank - 1), "+="),
        IR_Assignment(varname, 0)),
      IR_Assignment(IR_IV_FragmentOffsetPerBlock().resolveAccess(curRank), varname)
    )

    // interpolate face centered values towards cell centers
    if (tmpBufStag.isDefined)
      statements ++= interpStagField(tmpBufStag.get)

    statements
  }

  // write constant data once and reference the file containing the constant data afterwards
  override def writeXdmfElemOrReferenceConstants(stream : IR_VariableAccess, writeConsts : ListBuffer[IR_Statement], elemToRef : String, altCondition : Option[IR_Expression] = None) : ListBuffer[IR_Statement] = {
    val fragId = if (fmt != "XML") {
      fragIdCurRank(global = true)
    } else {
      IR_LoopOverFragments.defIt
    }
    val selectGrid = ListBuffer[IR_Expression](IR_StringConstant("Grid/Grid["), fragId + 1, IR_StringConstant("]")) // collection of grids (one for each fragment)

    ListBuffer(
      new IR_IfCondition(altCondition getOrElse IR_ConstantsWrittenToFile().isEmpty,
        /* true branch */
        writeConsts,
        /* false branch */
        ListBuffer[IR_Statement](
          printXdmfElement(stream, XInclude(href = IR_ConstantsWrittenToFile(), xpath = XPath(selectGrid, elemToRef) : _*) : _*)
        )
      )
    )
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

    val fragOff = if (tmpBufDest.blockwise) IR_LoopOverFragments.defIt * numCellsPerFrag else IR_IntegerConstant(0)

    val linearizedIdx = idxRange.linearizeIndex(IR_LoopOverDimensions.defIt(numDimsGrid))
    stmts += IR_LoopOverFragments(
      IR_IfCondition(IR_IV_IsValidForDomain(dataBuffer.domainIdx),
        IR_LoopOverDimensions(numDimsGrid, idxRange,
          IR_Assignment(
            IR_ArrayAccess(tmpBufDest, fragOff + linearizedIdx),
            mean))))

    stmts
  }

  val dimsDt : Int = dataBuffer.numDimsData - dataBuffer.numDimsGrid

  protected def fragIdCurRank(global : Boolean, rank : IR_Expression = curRank, fragIdx : IR_Expression = IR_LoopOverFragments.defIt) : IR_Expression = {
    if (Knowledge.mpi_enabled)
      (if (global) IR_IV_FragmentOffsetPerBlock().resolveAccess(curRank) else IR_IntegerConstant(0)) + fragIdx
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

  /* special handling for a variable number of frags */
  // specifies "fragment dimension" (i.e. how many fragments are written to a file)
  override def dimFrags(global : Boolean) : IR_Expression = if (!binaryFpp) {
    super.dimFrags(global)
  } else {
    // binary fpp: only root writes the xdmf file -> requires the number of valid frags for each rank
    IR_IV_NumValidFragsPerBlock(domainIndex).resolveAccess(curRank)
  }
  // contains expressions that calculate the seek pointer for each DataItem (used for raw binary files)
  override def seekpOffsets(global : Boolean, constsIncluded : Boolean) : ListBuffer[IR_Expression] = if (!binaryFpp) {
    super.seekpOffsets(global, constsIncluded)
  } else {
    // binary fpp: root needs to know the actual number of fragments of each rank to compute the file offsets correctly
    dataBuffers(constsIncluded).map(buf => if (global) buf.typicalByteSizeGlobal else buf.typicalByteSizeFrag * IR_IV_NumValidFragsPerBlock(domainIndex).resolveAccess(curRank))
  }

  // writes an Xdmf "Grid" element for each existing fragment
  override def writeXdmfGrid(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // write "Grid" for each fragment in 1) a single block: "XML" or 2) the whole domain
    val gridName = if (fmt != "XML") {
      IR_StringConstant("Grid") + IR_FunctionCall(IR_ExternalFunctionReference("std::to_string"), fragIdCurRank(global = true))
    } else {
      IR_StringConstant("Grid") + IR_FunctionCall(IR_ExternalFunctionReference("std::to_string"), IR_LoopOverFragments.defIt)
    }

    // write each subdomain with origins and spacing to the "global" file
    // this kind of loop is only used for "global" files in case of uniform meshes
    // since the handling is fundamentally different from other xdmf writers

    def loopOverFrags(statements : ListBuffer[IR_Statement]) = if (fmt == "XML") {
      IR_LoopOverFragments(IR_IfCondition(IR_IV_IsValidForDomain(domainIndex), statements))
    } else {
      IR_ForLoop(
        IR_VariableDeclaration(IR_LoopOverFragments.defIt, 0),
        IR_Lower(IR_LoopOverFragments.defIt, IR_IV_NumValidFragsPerBlock(domainIndex).resolveAccess(curRank)),
        IR_PreIncrement(IR_LoopOverFragments.defIt),
        statements : _*
      )
    }

    val printSubdomains = loopOverFrags(
      ListBuffer[IR_Statement](
        printXdmfElement(stream, openGrid(gridName, "Uniform") : _*)) ++
        writeXdmfGeometry(stream, global) ++
        writeXdmfTopology(stream, global) ++
        writeXdmfAttributes(stream, global) :+
        printXdmfElement(stream, closeGrid))

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
    val buf = dataBuffer

    statements += printXdmfElement(stream, openAttribute(field.name, attributeType(field.gridDatatype), centeringType(buf.localization)))

    // handle non-scalar datatypes
    val joinDataItems = dimsDt > 0 && ioInterface != "fpp"
    val arrayIndexRange = buf.datatype.resolveFlattendSize
    val dimsHigherDimDatatype = if (dimsDt > 0) {
      IR_IntegerConstant(arrayIndexRange) +: buf.innerDimsPerFrag.dropRight(dimsDt) // reorder indices (e.g. for matrix: [x, y, 2, 1] -> [2, x, y])
    } else {
      buf.innerDimsPerFrag
    }
    val localDimsTotal = if (ioInterface == "fpp") {
      // in fpp schemes, all components are written at once in a IR_LoopOverDimensions
      dimsHigherDimDatatype
    } else {
      // otherwise, multidim. datatypes are output as multiple "scalar" components
      buf.innerDimsPerFrag
    }
    val dimsComponent = if (joinDataItems) {
      localDimsTotal.dropRight(dimsDt) // select a component of the datatype
    } else {
      localDimsTotal // already correctly formatted
    }

    // introduce "Function DataItem" to combine (join) components of non-scalar datatypes
    if (joinDataItems) {
      val function = "JOIN(" + (0 until arrayIndexRange).map("$" + _).mkString(",") + ")"
      statements += printXdmfElement(stream, openDataItemFunction(dimsHigherDimDatatype, function) : _*)
    }

    val numComponents = if (joinDataItems) arrayIndexRange else 1
    (0 until numComponents).foreach(component => {
      // depending on format and file layout, select the appropriate portion from the "heavy data" file for each fragment
      if ((ioInterface == "hdf5") || (ioInterface == "mpiio" && buf.canonicalOrder)) {
        // global file is hdf5 or laid out canonically -> select portion via "hyperslabs"
        val startIndexGlobal = if (buf.canonicalOrder) {
          // replace last indices (of the multidim datatype) with the component selection
          val start = buf.canonicalStartIndexGlobal((0 until field.numDimsGrid).map(dim => fragIndexCurRank(dim)))
          accessComponent(component, field.gridDatatype, start.dropRight(dimsDt)) // e.g. [global x, global y, 2, 1] -> [global x, global y, 1, 1]
        } else {
          // replace indices at the end (except "fragment dimension") with the component selection
          val start = buf.fragmentwiseStartIndexGlobal(fragIdCurRank(global = true))
          accessComponent(component, field.gridDatatype, start.dropRight(dimsDt + 1)) :+ start.last // e.g. [x, y, z, 3, 1, fragment] -> [x, y, z, 1, 1, fragment]
        }

        // conditionally add "fragment dimension (1)" to local dims
        val localDimsComponent = dimsComponent ++ ListBuffer.fill(dimsDt)(IR_IntegerConstant(1)) // extract values for one component
        val count = if (!buf.canonicalOrder) localDimsComponent :+ IR_IntegerConstant(1) else localDimsComponent
        val stride = IR_DataBuffer.handleFragmentDimension(buf.canonicalOrder, buf.accessBlockwise, buf.stride, 1, orderKJI = false)

        statements += printXdmfElement(stream, openDataItemHyperslab(dimsComponent) : _*)
        statements += printXdmfElement(stream, dataItemHyperslabSelection(startIndexGlobal, stride, count) : _*)
        statements += printXdmfElement(stream, dataItemHyperslabSource(buf.datatype.resolveBaseDatatype, buf.globalDims, printFilename(stream, dataset), offsetDataBuffer) : _*)
      } else {
        val offsetComponent = component * dimsComponent.reduce(_ * _) * buf.datatype.resolveBaseDatatype.typicalByteSize // for non-scalar datatypes: skip to current component
        val seekp = offsetDataBuffer + offsetComponent + (if (fmt == "Binary") fragIdCurRank(global) * buf.typicalByteSizeFrag else 0) // field data laid out fragment-wise -> calc. offset to each fragment

        // file laid out fragment-wise -> select portion via offsets
        statements += printXdmfElement(stream, openDataItem(field.resolveBaseDatatype, dimsComponent, seekp) : _*)
        if (fmt == "XML") {
          val printComponents = ListBuffer[IR_Expression]()
          printComponents += "std::scientific"
          printComponents += indentData
          printComponents ++= buf.getIndicesMultiDimDatatypes.flatMap(idx => {
            if (buf.accessBlockwise) {
              // HACK: already within a fragment loop -> drop fragment loop from temp. buffer and access values with index from outer loop
              val accessIndices = idx.toExpressionIndex.indices.dropRight(1) :+ IR_LoopOverFragments.defIt
              List(buf.getAccess(IR_ExpressionIndex(accessIndices)), separator)
            } else {
              List(buf.getAccess(idx), separator)
            }
          }).dropRight(1) // last separator
          printComponents += IR_Print.newline

          statements += IR_LoopOverDimensions(numDimsGrid,
            IR_ExpressionIndexRange(
              IR_ExpressionIndex(buf.numDimsGridRange.map(dim => buf.beginIndices(dim) - Duplicate(buf.referenceOffset(dim)) : IR_Expression).toArray),
              IR_ExpressionIndex(buf.numDimsGridRange.map(dim => buf.endIndices(dim) - Duplicate(buf.referenceOffset(dim)) : IR_Expression).toArray)),
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

  // in this implementation, a xdmf "grid" is specified for each fragment -> calc. local number of nodes
  def nodeDims : ListBuffer[IR_Expression] = (0 until numDimsGrid).map(d => Knowledge.domain_fragmentLengthAsVec(d) * (1 << level) + 1 : IR_Expression).to[ListBuffer]

  def loopOverRanks(body : IR_Statement*) = IR_ForLoop(
    IR_VariableDeclaration(curRank, 0),
    IR_Lower(curRank, Knowledge.mpi_numThreads),
    IR_PreIncrement(curRank),
    body.to[ListBuffer])

  // VisIt needs every subdomain to be referenced explicitly whereas in ParaView it is possible to reference a process's "grid collection"
  override def refPiecesXml(stream : IR_VariableAccess) : ListBuffer[IR_Statement] = {
    if (!Knowledge.parIO_vis_generateVisItFiles) {
      super.refPiecesXml(stream)
    } else {
      ListBuffer(
        loopOverRanks( // ref each process's grid
          IR_ForLoop( // if a process has a collection of grids -> ref each
            IR_VariableDeclaration(IR_LoopOverFragments.defIt, 0),
            IR_Lower(IR_LoopOverFragments.defIt, IR_IV_NumValidFragsPerBlock(domainIndex).resolveAccess(curRank)),
            IR_PreIncrement(IR_LoopOverFragments.defIt),
            printXdmfElement(stream,
              XInclude(href = buildFilenamePiece(noPath = true, rank = curRank).toPrint,
                xpath = ListBuffer(IR_StringConstant("/Xdmf/Domain/Grid/Grid["), IR_LoopOverFragments.defIt + 1 : IR_Expression, IR_StringConstant("]")) : _*) : _*)
          )))
    }
  }
}

// communicate fragment info to root at startup to be able to write the Xdmf file solely on root
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

case class IR_IV_FragmentOffsetPerBlock() extends IR_UnduplicatedVariable {
  def resolveAccess(curRank : IR_Expression) : IR_Expression = IR_ArrayAccess(this, curRank)
  override def resolveName() = s"fragmentOffsetOnRoot"
  override def resolveDatatype() = IR_ArrayDatatype(IR_IntegerDatatype, Knowledge.mpi_numThreads)
}

object IR_PrintXdmfStructured {
  var firstCall = true
  var gatherFuncName = "gatherFragInfoRoot"

  def gatherFragInfoRoot(fmt : String) : Unit = {
    if (firstCall && fmt != "XML" && Knowledge.mpi_enabled) {
      // gather info on root
      StateManager.findFirst[IR_DomainFunctions]().get.functions foreach {
        case func : IR_PlainFunction if func.name == "initGeometry" =>
          firstCall = false

          func.body ++= (0 until Knowledge.dimensionality).to[ListBuffer].map(dim =>
            new MPI_Gather(
              IR_AddressOf(IR_IV_FragmentIndex(dim, 0)),
              IR_AddressOf(IR_IV_FragmentIndexPerBlock(dim).resolveAccess(0, 0)),
              IR_IntegerDatatype,
              Knowledge.domain_numFragmentsPerBlock))
          func.body ++= (0 until Knowledge.dimensionality).map(dim =>
            new MPI_Gather(
              IR_AddressOf(IR_IV_FragmentPositionBegin(dim, 0)),
              IR_AddressOf(IR_IV_FragmentPosBeginPerBlock(dim).resolveAccess(0, 0)),
              IR_RealDatatype,
              Knowledge.domain_numFragmentsPerBlock))
        case _                                                      =>
      }

      if (firstCall)
        Logger.error("Did not find domain function: \"initGeometry\".")
    }
  }
}


