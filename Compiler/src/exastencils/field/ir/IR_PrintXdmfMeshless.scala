package exastencils.field.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.grid.ir._
import exastencils.io.ir._
import exastencils.logger.Logger
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.IR_PrintXdmf

case class IR_PrintXdmfMeshless(
    var filename : IR_Expression,
    var field : IR_Field,
    var slot : IR_Expression,
    var ioMethod : IR_Expression,
    var includeGhostLayers : Boolean,
    var dataset : IR_Expression,
    var binaryFpp : Boolean,
    var resolveId : Int) extends IR_PrintXdmf(ioMethod, binaryFpp) {

  def datasetCoords : ListBuffer[IR_Expression] = (0 until numDimsGrid).map(d => IR_StringConstant("/constants/" + ('X' + d).toChar.toString) : IR_Expression).to[ListBuffer]

  // validate params
  if (includeGhostLayers) {
    Logger.error("Ghost layer visualization is currently unsupported for IR_PrintXdmfMeshless!")
  }
  if (numDimsGrid < 2) {
    Logger.error("IR_PrintXdmfMeshless is only usable for 2D/3D cases.")
  }

  // we always have the association: vertex position <-> field data. values do not need to be output canonically
  override def canonicalFileLayout : Boolean = false

  override def domainIndex : Int = field.domain.index

  override def centeringType(localization : IR_Localization) = "Node" // values are mapped to the "Polyvertex" topology

  val dataBufferField = IR_DataBuffer(field, slot, includeGhostLayers, None, Some(dataset), canonicalFileLayout)

  val dataBuffersVertexPos : ListBuffer[IR_DataBuffer] = {
    val faceDir = getFaceDir(field.localization)

    (0 until numDimsGrid).to[ListBuffer].map(dim => {
      val dataBufSources : (IR_Field, IR_IV_TemporaryBuffer) = field.localization match {
        case IR_AtNode              => (IR_VF_NodePositionAsVec.find(level).associatedField, nodePositionsBuf(dim))
        case IR_AtCellCenter        => (IR_VF_CellCenterAsVec.find(level).associatedField, cellCentersBuf(dim))
        case IR_AtFaceCenter(`dim`) => (IR_VF_NodePositionAsVec.find(level).associatedField, facePositionsBuf(faceDir)(dim))
        case IR_AtFaceCenter(_)     => (IR_VF_CellCenterAsVec.find(level).associatedField, facePositionsBuf(faceDir)(dim))
      }

      if (!Knowledge.grid_isAxisAligned && faceDir < 0) {
        // use associated field of vf directly
        IR_DataBuffer(dataBufSources._1, None, Some(datasetCoords(dim)), dim, 0, canonicalFileLayout)
      } else {
        // the vf's associated field doesn't have a suitable dimensionality -> create temp. buffer with positions depending on localization
        IR_DataBuffer(dataBufSources._2, IR_IV_ActiveSlot(field), None, Some(datasetCoords(dim)))
      }
    })
  }

  override def dataBuffersConst : ListBuffer[IR_DataBuffer] = dataBuffersVertexPos

  override def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] = {
    (if (constsIncluded) dataBuffersVertexPos else ListBuffer()) :+ dataBufferField
  }

  override def stmtsForPreparation : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements ++= IR_IV_FragmentInfo.init(
      domainIndex,
      // only calculate fragmentOffset for global filespaces
      calculateFragOffset = ioInterface != "fpp"
    )

    // conditionally init buffer with the corresponding field's positions
    if (fmt != "XML") {
      (0 until numDimsGrid).to[ListBuffer].foreach { dim =>
        val initBuf = field.localization match {
          case IR_AtNode          => initNodePosBuf(dim)
          case IR_AtCellCenter    => initCellCenterBuf(dim)
          case IR_AtFaceCenter(_) => initFacePosBuf(getFaceDir(field.localization))(dim)
        }
        statements ++= initBuf
      }
    }

    statements
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

  override def writeXdmfGeometry(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements += printXdmfElement(stream, openGeometry((0 until numDimsGrid).map(d => ('X' + d).toChar.toString).mkString("_"))) // positions are not interleaved
    for (d <- 0 until field.numDimsGrid) {
      val buf = dataBuffersVertexPos(d)
      val numVertices = IR_DataBuffer.handleFragmentDimension(buf.canonicalOrder, buf.accessBlockwise, buf.innerDimsLocal, dimFrags(global), orderKJI = false)

      statements += printXdmfElement(stream, openDataItem(IR_RealDatatype, numVertices, getSeekp(global)) : _*)
      statements += (if (fmt == "XML") {
        IR_LoopOverFragments(
          IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
            IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => field.layout.idxById("DLB", dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression)),
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => field.layout.idxById("DRE", dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression))),
              IR_Print(stream, indentData, getPos(field.localization, level, d), IR_Print.newline))),
          IR_Print(stream, IR_Print.flush))
      } else {
        printFilename(stream, datasetCoords(d))
      })
      statements += printXdmfElement(stream, closeDataItem)
    }
    statements += printXdmfElement(stream, closeGeometry)

    writeXdmfElemOrReferenceConstants(stream, statements, elemToRef = "Geometry")
  }

  override def writeXdmfTopology(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    val buf = dataBuffersVertexPos.head
    val numVertices = IR_DataBuffer.handleFragmentDimension(buf.canonicalOrder, buf.accessBlockwise, buf.innerDimsLocal, dimFrags(global), orderKJI = false)

    statements += printXdmfElement(stream, openTopology("Polyvertex", numVertices, Some("1")) : _*)
    statements += printXdmfElement(stream, closeTopology)

    statements
  }

  override def writeXdmfAttributes(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    val buf = dataBufferField
    val seekp = getSeekp(global)

    // handle multidim. field datatypes
    val dimsDt = field.layout.numDimsData - field.numDimsGrid
    val arrayIndexRange = field.gridDatatype.resolveFlattendSize
    val joinDataItems = dimsDt > 0 && ioInterface != "fpp"
    val dimsComponentFrag = buf.innerDimsPerFrag.dropRight(dimsDt)
    val dimsGridDatatypeFrag = if (dimsDt > 0)
      IR_IntegerConstant(arrayIndexRange) +: dimsComponentFrag // e.g. [x, y, z, 3, 1] -> [3, x, y, z]
    else
      buf.innerDimsPerFrag
    val totalDimsComponent = IR_DataBuffer.handleFragmentDimension(buf.canonicalOrder, buf.accessBlockwise, dimsComponentFrag, dimFrags(global), orderKJI = false)
    val totalDimsAttr = IR_DataBuffer.handleFragmentDimension(buf.canonicalOrder, buf.accessBlockwise, dimsGridDatatypeFrag, dimFrags(global), orderKJI = false)

    statements += printXdmfElement(stream, openAttribute(name = field.name, tpe = attributeType(field.gridDatatype), ctr = "Node"))
    if (!joinDataItems) {
      // non-scalar datatypes are already correctly formatted (i.e. components are output in an interleaved way)
      statements += printXdmfElement(stream, openDataItem(field.resolveBaseDatatype, totalDimsAttr, seekp) : _*)
      statements += (if (fmt == "XML") {
        // incorporate field values into the Xdmf file via I/O handler
        val handler = ioHandler(constsIncluded = true, filenamePieceFpp).asInstanceOf[IR_FileAccess_FPP]
        IR_LoopOverFragments(
          IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
            handler.printBufferAscii(buf, stream, condition = true, separator, indent = Some(indentData))))
      } else {
        printFilename(stream, dataset) // reference binary file
      })
      statements += printXdmfElement(stream, closeDataItem)
    } else {
      // handle non-scalar datatypes via hyperslab selection and Xdmf's "JOIN" funcions
      val function = "JOIN(" + (0 until arrayIndexRange).map("$" + _).mkString(",") + ")"
      statements += printXdmfElement(stream, openDataItemFunction(totalDimsAttr, function) : _*)
      (0 until arrayIndexRange).foreach(component => {
        val countComponent = dimsComponentFrag ++ ListBuffer.fill(dimsDt)(IR_IntegerConstant(1)) // extract values for one component
        val startComponent = accessComponent(component, field.gridDatatype, dimsComponentFrag.map(_ => IR_IntegerConstant(0))) // start at "current" component
        val count = IR_DataBuffer.handleFragmentDimension(buf.canonicalOrder, buf.accessBlockwise, countComponent, IR_IV_TotalNumFrags(domainIndex), orderKJI = false)
        val start = IR_DataBuffer.handleFragmentDimension(buf.canonicalOrder, buf.accessBlockwise, startComponent, 0, orderKJI = false)
        val stride = IR_DataBuffer.handleFragmentDimension(buf.canonicalOrder, buf.accessBlockwise, buf.stride, 1, orderKJI = false)

        statements += printXdmfElement(stream, openDataItemHyperslab(totalDimsComponent) : _*)
        statements += printXdmfElement(stream, dataItemHyperslabSelection(start, stride, count) : _*)
        statements += printXdmfElement(stream, dataItemHyperslabSource(field.resolveBaseDatatype, buf.globalDims, printFilename(stream, dataset), seekp) : _*)
        statements += printXdmfElement(stream, closeDataItem)
      })
      statements += printXdmfElement(stream, closeDataItem)
    }
    statements += printXdmfElement(stream, closeAttribute)

    statements
  }

  override def numDimsGrid : Int = field.layout.numDimsGrid
  override def numFields : Int = 1
  override def level : Int = field.level

  // unused for this implementation
  override def someCellField : IR_Field = ???
  override def numCellsPerFrag : IR_Expression = ???
  override def numCells_x : Int = ???
  override def numCells_y : Int = ???
  override def numCells_z : Int = ???
  override def connectivityForCell(global : Boolean) : ListBuffer[IR_Expression] = ???
  override def nodeOffsets : ListBuffer[IR_ConstIndex] = ???
}
