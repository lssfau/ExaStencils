package exastencils.field.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_VariableAccess
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.grid.ir.IR_AtNode
import exastencils.grid.ir.IR_VF_NodePositionAsVec
import exastencils.io.ir.IR_DataBuffer
import exastencils.io.ir.IR_IV_FragmentInfo
import exastencils.logger.Logger
import exastencils.util.ir.IR_Print

case class IR_PrintXdmfNonUniform_NonAA(
    var filename : IR_Expression,
    var field : IR_Field,
    var slot : IR_Expression,
    var ioMethod : IR_Expression,
    var includeGhostLayers : Boolean,
    var dataset : IR_Expression,
    var binaryFpp : Boolean,
    var canonicalFileLayout : Boolean,
    var resolveId : Int) extends IR_PrintXdmfRectilinear(ioMethod, binaryFpp) {

  def datasetCoords : ListBuffer[IR_Expression] = (0 until numDimsGrid).map(d => IR_StringConstant("/constants/" + ('X' + d).toChar.toString) : IR_Expression).to[ListBuffer]

  // validate params
  if (includeGhostLayers) {
    includeGhostLayers = false
    Logger.error("Ghost layer visualization is currently unsupported for IR_PrintXdmfNonUniform_NonAA!")
  }
  if (numDimsGrid < 2) {
    Logger.error("IR_PrintXdmfNonUniform_NonAA is only usable for 2D/3D cases.")
  }

  override def domainIndex : Int = field.domain.index

  val dataBuffersNodePos : ListBuffer[IR_DataBuffer] = (0 until numDimsGrid).to[ListBuffer].map(dim =>
    IR_DataBuffer(IR_VF_NodePositionAsVec.find(level).associatedField, None, Some(datasetCoords(dim)), dim, canonicalFileLayout))


  override def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] = {
    (if (constsIncluded) dataBuffersNodePos else ListBuffer()) :+ dataBuffer
  }

  override def stmtsForPreparation : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // TODO do once
    if (fmt != "XML" || Knowledge.mpi_enabled) {
      statements ++= communicateFragIndexToRoot
      statements += communicateFragIdToRoot
    }

    statements ++= IR_IV_FragmentInfo.init(dataBuffer.domainIdx)

    // interpolate face centered values towards cell centers
    if (tmpBufStag.isDefined)
      statements ++= interpStagField(tmpBufStag.get)

    statements
  }

  override def writeXdmfGeometry(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements += printXdmfElement(stream, openGeometry((0 until numDimsGrid).map(d => ('X'+d).toChar.toString).mkString("_"))) // positions are not interleaved
    for (d <- 0 until field.numDimsGrid) {
      val buf = dataBuffersNodePos(d)
      val offsetDataBuffer = getSeekp(global)

      if ((ioInterface == "hdf5") || (ioInterface == "mpiio" && buf.canonicalOrder)) {
        // global file is hdf5 or laid out canonically -> select portion via "hyperslabs"
        val startIndexGlobal = if (buf.canonicalOrder) {
          buf.canonicalStartIndexGlobal((0 until field.numDimsGrid).map(dim => fragIndexCurRank(dim)))
        } else {
          buf.fragmentwiseStartIndexGlobal(fragIdCurRank(global = true))
        }
        val count = if (!buf.canonicalOrder) buf.innerDimsPerFrag :+ IR_IntegerConstant(1) else buf.innerDimsPerFrag
        val stride = IR_DataBuffer.handleFragmentDimension(buf, buf.stride, 1, orderKJI = false)

        statements += printXdmfElement(stream, openDataItemHyperslab(buf.innerDimsLocal) : _*)
        statements += printXdmfElement(stream, dataItemHyperslabSelection(startIndexGlobal, stride, count) : _*)
        statements += printXdmfElement(stream, dataItemHyperslabSource(buf.datatype.resolveBaseDatatype, buf.globalDims, printFilename(stream, datasetCoords(d)), offsetDataBuffer) : _*)
      } else {
        val seekp = offsetDataBuffer + (if (fmt == "Binary") fragIdCurRank(global) * buf.typicalByteSizeFrag else 0) // field data laid out fragment-wise -> calc. offset to each fragment

        // file laid out fragment-wise -> select portion via offsets
        statements += printXdmfElement(stream, openDataItem(field.resolveBaseDatatype, buf.innerDimsPerFrag, seekp) : _*)
        if (fmt == "XML") {
          statements += IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
            loopOverNodes(ListBuffer(IR_Print(stream, indentData, getPos(IR_AtNode, level, d), IR_Print.newline))))
          statements += IR_Print(stream, IR_Print.flush)
        } else {
          statements += printFilename(stream, datasetCoords(d))
        }
      }
      statements += printXdmfElement(stream, closeDataItem)
    }

    statements += printXdmfElement(stream, closeGeometry)

    writeOrReferenceConstants(stream, statements, elemToRef = "Geometry")
  }

  override def writeXdmfTopology(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    // in this implementation, a xdmf "grid" is specified for each fragment -> calc. local number of nodes
    val nodeDims = (0 until numDimsGrid).map(d => Knowledge.domain_fragmentLengthAsVec(d) * (1 << level) + 1 : IR_Expression).to[ListBuffer]

    var statements : ListBuffer[IR_Statement] = ListBuffer()
    statements += printXdmfElement(stream, openTopology(if (numDimsGrid == 3) "3DSMesh" else "2DSMesh", nodeDims) : _*)
    statements += printXdmfElement(stream, closeTopology)

    statements
  }

  override def numDimsGrid : Int = field.layout.numDimsGrid
  override def numFields : Int = 1
  override def level : Int = field.level

  // unused for this implementation
  override def someCellField : IR_Field = ???
  override def connectivityForCell(global : Boolean) : ListBuffer[IR_Expression] = ???
  override def nodeOffsets : ListBuffer[IR_ConstIndex] = ???
}
