package exastencils.field.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_ForLoop
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_Lower
import exastencils.base.ir.IR_PreIncrement
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_ToInt
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_DomainCollection
import exastencils.domain.ir.IR_DomainFromAABB
import exastencils.domain.ir.IR_IV_FragmentPositionBegin
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.grid.ir.IR_VF_CellWidthPerDim
import exastencils.io.ir.IR_DataBuffer
import exastencils.io.ir.IR_FileAccess_FPP
import exastencils.io.ir.IR_IV_FragmentInfo
import exastencils.logger.Logger
import exastencils.util.ir.IR_AABB
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.IR_PrintXdmf

case class IR_PrintXdmfUniform(
    var filename : IR_Expression,
    var field : IR_Field,
    var slot : IR_Expression,
    var ioMethod : IR_Expression,
    var includeGhostLayers : Boolean,
    var dataset : IR_Expression,
    var binaryFpp : Boolean,
    var canonicalFileLayout : Boolean) extends IR_PrintXdmf(ioMethod, binaryFpp) {

  // TODO: test for serial applications

  // validate params
  if (includeGhostLayers) {
    includeGhostLayers = false
    Logger.warn("Ghost layer visualization is currently unsupported for IR_PrintXdmfUniform!")
  }
  if (numDimsGrid < 2) {
    Logger.error("IR_PrintXdmfUniform is only usable for 2D/3D cases.")
  }

  val dataBuffer = IR_DataBuffer(field, slot, includeGhostLayers, None, Some(dataset), canonicalFileLayout)
  override def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] = ListBuffer(dataBuffer)

  override def stmtsForPreparation : ListBuffer[IR_Statement] = IR_IV_FragmentInfo.init(
    dataBuffer.domainIdx,
    // in file-per-process, each rank writes its own domain piece individually -> fragOffset = 0
    calculateFragOffset = ioInterface != "fpp"
  )

  // only root writes the xdmf file (except for "XML" format) -> fragment position for other processes must be calculated
  private def globalSize : IR_AABB = IR_DomainCollection.getByIdentifier("global").get.asInstanceOf[IR_DomainFromAABB].aabb
  private def fragWidth(dim : Int) : Double = globalSize.width(dim) / Knowledge.domain_rect_numFragsTotalAsVec(dim)
  private def localFragIndex(dim : Int) : IR_Expression = (IR_LoopOverFragments.defIt / (0 until dim).map(Knowledge.domain_rect_numFragsPerBlockAsVec(_)).product) Mod Knowledge.domain_rect_numFragsPerBlockAsVec(dim)
  private def rankIndex(dim : Int) : IR_Expression = (curRank / (0 until dim).map(dim => Knowledge.domain_rect_numBlocksAsVec(dim)).product) Mod Knowledge.domain_rect_numBlocksAsVec(dim)

  private def fragPos(dim : Int) : IR_Expression = (rankIndex(dim) * Knowledge.domain_rect_numFragsPerBlockAsVec(dim) + 0.5 + localFragIndex(dim)) * fragWidth(dim) + globalSize.lower(dim)
  private def fragPosBegin(dim : Int) : IR_Expression = fragPos(dim) - 0.5 * fragWidth(dim)
  private def fragIndex(dim : Int) : IR_Expression = IR_ToInt((fragPos(dim) - globalSize.lower(dim)) / fragWidth(dim))

  // assumes uniform grids always have "valid" frags
  private def fragId(global : Boolean) : IR_Expression = IR_LoopOverFragments.defIt + (if (global) curRank * Knowledge.domain_numFragmentsPerBlock else 0)

  private def globalIndexOnRoot : ListBuffer[IR_Expression] = if (canonicalFileLayout) {
    dataBuffer.canonicalStartIndexGlobal(dataBuffer.numDimsGridRange.map(fragIndex))
  } else {
    dataBuffer.fragmentwiseStartIndexGlobal(fragId(global = true))
  }

  // specialization for file-per-process: fields are written fragment-after-fragment (i.e. no canonical layout)
  // -> an Xdmf "Grid" element with origin and spacing must be specified for each fragment to match the layout of the written field data
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
      IR_IfCondition(IR_IV_IsValidForDomain(field.domain.index),
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

  // HACK: VisIt needs every subdomain to be referenced explicitly whereas in ParaView it is possible to reference a process's "grid collection"
  override def refPiecesXml(stream : IR_VariableAccess) : ListBuffer[IR_Statement] = {
    if (!Knowledge.parIO_generateVisItFiles) {
      super.refPiecesXml(stream)
    } else {
      (0 until Knowledge.mpi_numThreads).flatMap(r => { // ref each process's grid
        (0 until Knowledge.domain_numFragmentsPerBlock).map(f => { // if a process has a collection of grids -> ref each
          printXdmfElement(stream,
            XInclude(href = buildFilenamePiece(noPath = true, rank = IR_IntegerConstant(r)).toPrint,
              xpath = ListBuffer(IR_StringConstant("/Xdmf/Domain/Grid/Grid["), f : IR_Expression, IR_StringConstant("]")) : _*) : _*)
        })
      }).to[ListBuffer]
    }
  }

  override def writeXdmfGeometry(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    /* handling for issue Paraview/VisIt:
      - when specifying the origin/spacing for a CoRectMesh, the order of these values depends on the vis. tool
      - https://gitlab.kitware.com/paraview/paraview/-/issues/13274 and https://gitlab.kitware.com/vtk/vtk/-/issues/17886
    */
    def handleOrderCoRectMesh(seq: Seq[IR_Expression]) : ListBuffer[IR_Expression] = (if (Knowledge.parIO_generateVisItFiles) seq.reverse else seq).to[ListBuffer]

    val origin = handleOrderCoRectMesh((0 until numDimsGrid).map(dim =>
      if (fmt != "XML") {
        fragPosBegin(dim)
      } else {
        IR_IV_FragmentPositionBegin(dim) : IR_Expression
      })
    )
    statements += printXdmfElement(stream, openGeometry("Origin_" + (0 until numDimsGrid).map(d => "D" + ('x'+d).toChar.toString).mkString("")))
    statements += printXdmfElement(stream, openDataItem(IR_RealDatatype, dims = ListBuffer(numDimsGrid), name = "origin", altFmt = Some("XML")) : _*)
    statements += IR_Print(stream, indentData +: separateSequenceAndFilter(origin) :+ IR_Print.newline)
    statements += printXdmfElement(stream, closeDataItem)

    val spacing = handleOrderCoRectMesh((0 until numDimsGrid).map(d => IR_VF_CellWidthPerDim.access(level, d, IR_ExpressionIndex(0)) : IR_Expression))
    statements += printXdmfElement(stream, openDataItem(IR_RealDatatype, dims = ListBuffer(numDimsGrid), name = "spacing", altFmt = Some("XML")) : _*)
    statements += IR_Print(stream, indentData +: separateSequenceAndFilter(spacing) :+ IR_Print.newline)
    statements += printXdmfElement(stream, closeDataItem)
    statements += printXdmfElement(stream, closeGeometry)

    statements
  }

  override def writeXdmfTopology(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    // in this implementation, a xdmf "grid" is specified for each fragment -> calc. local number of nodes
    val nodeDims = (0 until numDimsGrid).map(d => Knowledge.domain_fragmentLengthAsVec(d) * (1 << level) + 1 : IR_Expression).to[ListBuffer]

    var statements : ListBuffer[IR_Statement] = ListBuffer()
    statements += printXdmfElement(stream, openTopology(if (numDimsGrid == 3) "3DCoRectMesh" else "2DCoRectMesh", nodeDims) : _*)
    statements += printXdmfElement(stream, closeTopology)

    statements
  }

  override def writeXdmfAttributes(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    val seekp : IR_Expression = if (fmt == "Binary") fragId(global) * dataBuffer.typicalByteSizeFrag else 0 // field data laid out fragment-wise -> calc. offset to each fragment

    // depending on format and file layout, select the appropriate portion from the "heavy data" file for each fragment
    statements += printXdmfElement(stream, openAttribute(field.name, attributeType(dataBuffer.datatype), centeringType(dataBuffer.localization)))
    if ( (ioInterface == "hdf5") || (ioInterface == "mpiio" && canonicalFileLayout) ) {
      // global file is hdf5 or laid out canonically -> select portion via "hyperslabs"
      val globalDims = dataBuffer.globalDims
      val startIndexGlobal = globalIndexOnRoot
      // conditionally add "fragment dimension" to local dims (for non-canonical layouts)
      val localDims = IR_DataBuffer.handleFragmentDimension(dataBuffer, dataBuffer.innerDimsLocal, 1, orderKJI = false)
      val stride = IR_DataBuffer.handleFragmentDimension(dataBuffer, dataBuffer.stride, 1, orderKJI = false)
      statements += printXdmfElement(stream, openDataItemHyperslab(dataBuffer.innerDimsLocal) : _*)
      statements += printXdmfElement(stream, dataItemHyperslabSelection(startIndexGlobal, stride, localDims) : _*)
      statements += printXdmfElement(stream, dataItemHyperslabSource(dataBuffer.datatype.resolveBaseDatatype, globalDims, printFilename(stream, dataset)) : _*)
    } else {
      // file laid out fragment-wise -> select portion via seek pointers
      val localDims = dataBuffer.innerDimsLocal
      statements += printXdmfElement(stream, openDataItem(field.resolveBaseDatatype, localDims, seekp) : _*)
      if (fmt == "XML") {
        val handler = ioHandler(constsIncluded = false, filenamePieceFpp).asInstanceOf[IR_FileAccess_FPP]
        for (bufIdx <- handler.dataBuffers.indices)
          statements ++= handler.printKernel(stream, bufIdx, Some(indentData))
      } else {
        statements += printFilename(stream, dataset)
      }
    }
    statements += printXdmfElement(stream, closeDataItem)
    statements += printXdmfElement(stream, closeAttribute)

    statements
  }

  // no constants to be reduced -> only write data
  override def writeDataAndSetConstFile() : ListBuffer[IR_Statement] = {
    if (fmt != "XML") {
      writeData(constsIncluded = false)
    } else {
      ListBuffer() // data already incorporated in xdmf file
    }
  }

  override def numDimsGrid : Int = field.layout.numDimsGrid
  override def numFields : Int = 1
  override def level : Int = field.level

  // unused for this implementation
  override def connectivityForCell(global : Boolean) : ListBuffer[IR_Expression] = ???
  override def dimsPositionsFrag : ListBuffer[IR_IntegerConstant] = ???
  override def numCellsPerFrag : Int = ???
  override def numCells_x : Int = ???
  override def numCells_y : Int = ???
  override def numCells_z : Int = ???
  override def someCellField : IR_Field = ???
  override def nodeOffsets : ListBuffer[IR_ConstIndex] = ???
}
