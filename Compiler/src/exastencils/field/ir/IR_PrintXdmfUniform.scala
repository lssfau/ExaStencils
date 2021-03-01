package exastencils.field.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_VariableAccess
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_IV_FragmentPositionBegin
import exastencils.grid.ir.IR_VF_CellWidthPerDim
import exastencils.io.ir.IR_DataBuffer
import exastencils.io.ir.IR_IV_FragmentInfo
import exastencils.logger.Logger
import exastencils.util.ir.IR_Print

/// IR_PrintXdmfUniform
// provides visualization of scalar and vector datatypes on an uniform mesh
// usable with following I/O interfaces: file-per-process, hdf5, mpiio
// supports file layouts: canonical and fragment-wise ordering

/*
 NOTE, otherwise the visualization will break:
 - when using VisIt, enable "Knowledge.parIO_generateVisItFiles" flag
 - when using ParaView, disable "Knowledge.parIO_generateVisItFiles" flag
*/

case class IR_PrintXdmfUniform(
    var filename : IR_Expression,
    var field : IR_Field,
    var slot : IR_Expression,
    var ioMethod : IR_Expression,
    var includeGhostLayers : Boolean,
    var dataset : IR_Expression,
    var binaryFpp : Boolean,
    var canonicalFileLayout : Boolean,
    var resolveId : Int) extends IR_PrintXdmfRectilinear(ioMethod, binaryFpp) {

  override def numDimsGrid : Int = field.layout.numDimsGrid
  override def numFields : Int = 1
  override def level : Int = field.level

  // validate params
  if (includeGhostLayers) {
    includeGhostLayers = false
    Logger.error("Ghost layer visualization is currently unsupported for IR_PrintXdmfUniform!")
  }
  if (numDimsGrid < 2) {
    Logger.error("IR_PrintXdmfUniform is only usable for 2D/3D cases.")
  }

  override def domainIndex : Int = field.domain.index

  override def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] = ListBuffer(dataBuffer)

  override def stmtsForPreparation : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    // TODO do once
    if (fmt != "XML" || Knowledge.mpi_enabled) {
      stmts ++= communicateFragIndexToRoot
      stmts ++= communicateFragPosBeginToRoot
      stmts += communicateFragIdToRoot
    }

    stmts ++= IR_IV_FragmentInfo.init(dataBuffer.domainIdx)

    // interpolate face centered values towards cell centers
    if (tmpBufStag.isDefined)
      stmts ++= interpStagField(tmpBufStag.get)

    stmts
  }

  override def writeXdmfGeometry(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    /* handling for issue Paraview/VisIt:
      - when specifying the origin/spacing for a CoRectMesh, the order of these values depends on the vis. tool
      - https://gitlab.kitware.com/paraview/paraview/-/issues/13274 and https://gitlab.kitware.com/vtk/vtk/-/issues/17886
    */
    def handleOrderCoRectMesh(seq: Seq[IR_Expression]) : ListBuffer[IR_Expression] = (if (Knowledge.parIO_vis_generateVisItFiles) seq.reverse else seq).to[ListBuffer]

    val origin = handleOrderCoRectMesh((0 until numDimsGrid).map(dim =>
      if (fmt != "XML") {
        fragPosBeginCurRank(dim)
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

  override def writeDataAndSetConstFile() : ListBuffer[IR_Statement] = {
    val setConstFile = IR_ConstantsWrittenToFile().setFilename(IR_StringConstant("")) // no constants to be reduced -> set to empty string
    val write = if (fmt != "XML") {
      writeData(constsIncluded = false)
    } else {
      ListBuffer() // data already incorporated in xdmf file
    }
    setConstFile +: write
  }

  // unused for this implementation
  override def connectivityForCell(global : Boolean) : ListBuffer[IR_Expression] = ???
  override def someCellField : IR_Field = ???
  override def nodeOffsets : ListBuffer[IR_ConstIndex] = ???
}
