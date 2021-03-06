package exastencils.applications.ns.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir.IR_IV_ActiveSlot
import exastencils.grid.ir.IR_AtNode
import exastencils.io.ir._
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.postprocessing.xdmf.IR_PrintXdmf

/// IR_PrintXdmfNS
// 2D or 3D
// for a fixed number of fragments per block

case class IR_PrintXdmfNS(
    var filename : IR_Expression,
    level : Int,
    ioMethod : IR_Expression,
    var binaryFpp : Boolean,
    var resolveId : Int) extends IR_PrintXdmf(ioMethod) with IR_PrintVisualizationNS with IR_PrintFieldAsciiNS {

  // check if cell field conforms grid dims
  conformsGridDimensions(someCellField)

  def fieldnames : ListBuffer[String] = ListBuffer("vel", "p")

  // dataset names for hdf5
  def datasetCoords : ListBuffer[String] = ListBuffer("/constants/X", "/constants/Y", "/constants/Z")
  def datasetConnectivity = "/constants/Connectivity"
  def datasetFields : ListBuffer[String] = fieldnames.map(name => "/fieldData/" + name)

  override def stmtsForPreparation : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    // setup frag info and temp buffers
    if (fmt != "XML") {
      stmts ++= IR_IV_FragmentInfo.init(domainIndex)
      stmts ++= setupNodePositions()
      stmts ++= setupConnectivity(global = ioInterface != "fpp")
      stmts ++= setupVelocityBuf
    }

    stmts
  }

  override def writeXdmfGeometry(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements += printXdmfElement(stream, openGeometry("X_Y" + (if (numDimsGrid > 2) "_Z" else ""))) // nodePositions are not interleaved
    (0 until numDimsGrid).foreach(d => {
      statements += printXdmfElement(stream, openDataItem(IR_RealDatatype, dimsPositionsFrag :+ dimFrags(global), getSeekp(global)) : _*)
      statements ++= (if (fmt == "XML") {
        ListBuffer(IR_Print(stream, "std::scientific"),
          IR_LoopOverFragments(
            IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
              IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DLB", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression)),
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => 1 + someCellField.layout.idxById("DRE", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression))),
                IR_Print(stream, indentData, getPos(IR_AtNode, level, d), IR_Print.newline))),
            IR_Print(stream, IR_Print.flush)))
      } else {
        ListBuffer(printFilename(stream, datasetCoords(d)))
      })
      statements += printXdmfElement(stream, closeDataItem)
    })
    statements += printXdmfElement(stream, closeGeometry)

    writeXdmfElemOrReferenceConstants(stream, statements, elemToRef = "Geometry")
  }

  override def writeXdmfTopology(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements += printXdmfElement(stream, openTopology(if (numDimsGrid == 2) "Quadrilateral" else "Hexahedron", ListBuffer(numCellsPerFrag, dimFrags(global))) : _*)
    statements += printXdmfElement(stream, openDataItem(IR_IntegerDatatype, dimsConnectivityFrag :+ dimFrags(global), getSeekp(global)) : _*)
    statements += (if (fmt == "XML") {
      IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
          IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
            IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DLB", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression)),
            IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DRE", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression))),
            IR_Print(stream, indentData +: separateSequenceAndFilter(connectivityForCell(global = false)) :+ IR_Print.newline))),
        IR_Print(stream, IR_Print.flush))
    } else {
      printFilename(stream, datasetConnectivity)
    })
    statements += printXdmfElement(stream, closeDataItem)
    statements += printXdmfElement(stream, closeTopology)

    writeXdmfElemOrReferenceConstants(stream, statements, elemToRef = "Topology")
  }

  override def writeXdmfAttributes(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    fieldnames.zipWithIndex.foreach { case (fname, fid) =>
      val isVector = fname == "vel"
      val dimsFieldData = IR_IntegerConstant(if (isVector) numDimsGrid else 1)
      val dimsCellData = ListBuffer[IR_Expression](numCells_x, numCells_y, numCells_z)
      statements += printXdmfElement(stream, openAttribute(name = fname, tpe = if (isVector) "Vector" else "Scalar", ctr = "Cell"))
      statements += printXdmfElement(stream, openDataItem(someCellField.resolveBaseDatatype, dimsFieldData +: dimsCellData :+ dimFrags(global), getSeekp(global)) : _*)
      val printValsOrRefFile = if (fmt == "XML") {
        fname match {
          case "vel" => printVel(Some(stream), Some(indentData))
          case "p"   => printP(Some(stream), Some(indentData))
        }
      } else {
        ListBuffer(printFilename(stream, datasetFields(fid)))
      }
      statements ++= printValsOrRefFile
      statements += printXdmfElement(stream, closeDataItem)
      statements += printXdmfElement(stream, closeAttribute)
    }

    statements
  }

  override def dataBuffersConst : ListBuffer[IR_DataBuffer] = {
    val nodePos = nodePositionsBuf.zipWithIndex.map { case (buf, idx) =>
      IR_DataBuffer(buf, IR_IV_ActiveSlot(p), None, Some(IR_StringConstant(datasetCoords(idx))))
    }
    val con = IR_DataBuffer(connectivityBuf, IR_IV_ActiveSlot(p), None, Some(IR_StringConstant(datasetConnectivity)))

    nodePos :+ con
  }

  override def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] = {
    val fields = ListBuffer(
      IR_DataBuffer(velocityBuf, IR_IV_ActiveSlot(u), None, Some(IR_StringConstant(datasetFields.head))),
      IR_DataBuffer(p, IR_IV_ActiveSlot(p), includeGhosts = false, None, Some(IR_StringConstant(datasetFields(1))), canonicalOrder = false))

    if (constsIncluded) dataBuffersConst ++ fields else fields
  }

}
