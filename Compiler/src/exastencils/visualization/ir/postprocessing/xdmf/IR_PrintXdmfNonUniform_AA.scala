package exastencils.visualization.ir.postprocessing.xdmf

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir.IR_Field
import exastencils.grid.ir.IR_VF_NodePositionPerDim
import exastencils.io.ir.IR_DataBuffer
import exastencils.io.ir.IR_Iostream
import exastencils.util.ir.IR_Print

case class IR_PrintXdmfNonUniform_AA(
    var filename : IR_Expression,
    var field : IR_Field,
    var slot : IR_Expression,
    var ioMethod : IR_Expression,
    var includeGhostLayers : Boolean,
    var dataset : IR_Expression,
    var binaryFpp : Boolean,
    var canonicalFileLayout : Boolean,
    var resolveId : Int) extends IR_PrintXdmfStructured(ioMethod) with IR_Iostream {

  def datasetCoords : ListBuffer[IR_Expression] = (0 until numDimsGrid).map(d => IR_StringConstant("/constants/" + ('X' + d).toChar.toString) : IR_Expression).to[ListBuffer]

  override def domainIndex : Int = field.domain.index

  def nodePosAssocField(dim : Int) : IR_Field = IR_VF_NodePositionPerDim(level, field.domain, dim).associatedField

  val dataBuffersNodePos : ListBuffer[IR_DataBuffer] = (0 until numDimsGrid).to[ListBuffer].map(dim => {
    IR_DataBuffer(nodePosAssocField(dim), slot, includeGhostLayers, None, Some(datasetCoords(dim)), canonicalFileLayout)
  })

  override def dataBuffersConst : ListBuffer[IR_DataBuffer] = dataBuffersNodePos

  override def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] = {
    (if (constsIncluded) dataBuffersNodePos else ListBuffer()) :+ dataBuffer
  }

  override def writeXdmfGeometry(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements += printXdmfElement(stream, openGeometry((0 until numDimsGrid).map(d => 'V' + ('x' + d).toChar.toString).mkString)) // positions are not interleaved
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
        val stride = IR_DataBuffer.handleFragmentDimension(buf.canonicalOrder, buf.accessBlockwise, buf.stride, 1, orderKJI = false)

        statements += printXdmfElement(stream, openDataItemHyperslab(buf.innerDimsLocal) : _*)
        statements += printXdmfElement(stream, dataItemHyperslabSelection(startIndexGlobal, stride, count) : _*)
        statements += printXdmfElement(stream, dataItemHyperslabSource(buf.datatype.resolveBaseDatatype, buf.globalDims, printFilename(stream, datasetCoords(d)), offsetDataBuffer) : _*)
      } else {
        val seekp = offsetDataBuffer + (if (fmt == "Binary") fragIdCurRank(global) * buf.typicalByteSizeFrag else 0) // field data laid out fragment-wise -> calc. offset to each fragment

        // file laid out fragment-wise -> select portion via offsets
        statements += printXdmfElement(stream, openDataItem(field.resolveBaseDatatype, buf.innerDimsPerFrag, seekp) : _*)
        if (fmt == "XML") {
          statements += IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
            printBufferAscii(buf, stream, true, separator, indent = Some(indentData)))
          statements += IR_Print(stream, IR_Print.flush)
        } else {
          statements += printFilename(stream, datasetCoords(d))
        }
      }
      statements += printXdmfElement(stream, closeDataItem)
    }

    statements += printXdmfElement(stream, closeGeometry)

    writeXdmfElemOrReferenceConstants(stream, statements, elemToRef = "Geometry")
  }

  override def writeXdmfTopology(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = ListBuffer(
    printXdmfElement(stream, openTopology(if (numDimsGrid == 3) "3DRectMesh" else "2DRectMesh", nodeDims) : _*),
    printXdmfElement(stream, closeTopology)
  )

  override def numDimsGrid : Int = field.layout.numDimsGrid
  override def numFields : Int = 1
  override def level : Int = field.level

  // unused for this implementation
  override def someCellField : IR_Field = ???
  override def connectivityForCell(global : Boolean) : ListBuffer[IR_Expression] = ???
  override def nodeOffsets : ListBuffer[IR_ConstIndex] = ???
}
