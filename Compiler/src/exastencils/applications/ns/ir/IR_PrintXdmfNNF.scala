package exastencils.applications.ns.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_VariableAccess
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir.IR_IV_ActiveSlot
import exastencils.grid.ir.IR_AtNode
import exastencils.io.ir.IR_DataBuffer
import exastencils.io.ir.IR_IV_FragmentInfo
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.IR_PrintXdmf

/// IR_PrintXdmfNNF
// 2D or 3D
// for a fixed number of fragments per block

case class IR_PrintXdmfNNF(
    var filename : IR_Expression,
    level : Int,
    ioMethod : IR_Expression,
    binaryFpp : Boolean) extends IR_PrintXdmf(ioMethod, binaryFpp) with IR_PrintVisualizationNS with IR_PrintFieldAsciiNS {

  def fieldnames : ListBuffer[String] = ListBuffer("vel", "p", "rho", "mue", "gamma", "phi")

  // dataset names for hdf5
  def datasetCoords : ListBuffer[String] = ListBuffer("/constants/X", "/constants/Y", "/constants/Z")
  def datasetConnectivity = "/constants/Connectivity"
  def datasetFields : ListBuffer[String] = fieldnames.map(name => "/fieldData/" + name)

  override def stmtsForPreparation : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    // setup frag info and temp buffers
    if (fmt != "XML") {
      stmts ++= IR_IV_FragmentInfo.init(domainIndex)
      stmts ++= setupNodePositions
      stmts ++= setupConnectivity(global = ioInterface != "fpp")
      stmts ++= setupVelocityBuf
    }

    stmts
  }

  override def writeXdmfGeometry(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements += printXdmfElement(stream, openGeometry("X_Y" + (if (numDimsGrid > 2) "_Z" else ""))) // nodePositions are not interleaved
    for (d <- 0 until numDimsGrid) {
      statements += printXdmfElement(stream, openDataItem(IR_RealDatatype, dimsPositionsFrag :+ dimFrags(global), seekp = getSeekp(global)) : _*)
      val printValsOrRefFile = if (fmt == "XML") {
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
      }
      statements ++= printValsOrRefFile
      statements += printXdmfElement(stream, closeDataItem)
    }
    statements += printXdmfElement(stream, closeGeometry)

    writeOrReferenceConstants(stream, statements, elemToRef = "Geometry")
  }

  override def writeXdmfTopology(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements += printXdmfElement(stream, openTopology(if (numDimsGrid == 2) "Quadrilateral" else "Hexahedron", ListBuffer(numCellsPerFrag, dimFrags(global))) : _*)
    statements += printXdmfElement(stream, openDataItem(IR_IntegerDatatype, dimsConnectivityFrag :+ dimFrags(global), seekp = getSeekp(global)) : _*)
    val printValsOrRefFile = if (fmt == "XML") {
      IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
          IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
            IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DLB", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression)),
            IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DRE", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression))),
            IR_Print(stream, indentData +: separateSequenceAndFilter(connectivityForCell(global = false)) :+ IR_Print.newline))),
        IR_Print(stream, IR_Print.flush))
    } else {
      printFilename(stream, datasetConnectivity)
    }
    statements += printValsOrRefFile
    statements += printXdmfElement(stream, closeDataItem)
    statements += printXdmfElement(stream, closeTopology)

    writeOrReferenceConstants(stream, statements, elemToRef = "Topology")
  }

  override def writeXdmfAttributes(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    fieldnames.zipWithIndex.foreach { case (fname, fid) =>
      val isVector = fname == "vel"
      val dimsFieldData = IR_IntegerConstant(if (isVector) numDimsGrid else 1)
      val dimsCellData = ListBuffer[IR_Expression](numCells_z, numCells_y, numCells_x)
      statements += printXdmfElement(stream, openAttribute(name = fname, tpe = if (isVector) "Vector" else "Scalar", ctr = "Cell"))
      statements += printXdmfElement(stream, openDataItem(someCellField.resolveBaseDatatype, dimsFieldData +: dimsCellData :+ dimFrags(global), seekp = getSeekp(global)) : _*)
      val printValsOrRefFile = if (fmt == "XML") {
        fname match {
          case "vel"   => printVel(Some(stream), Some(indentData))
          case "p"     => printP(Some(stream), Some(indentData))
          case "rho"   => printRho(Some(stream), Some(indentData))
          case "mue"   => printMue(Some(stream), Some(indentData))
          case "gamma" => printGamma(Some(stream), Some(indentData))
          case "phi"   => printPhi(Some(stream), Some(indentData))
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

  override def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] = {
    val constants = nodePositionsBuf.zipWithIndex.map { case (buf, idx) =>
      IR_DataBuffer(buf, IR_IV_ActiveSlot(p), None, Some(IR_StringConstant(datasetCoords(idx)))) } :+
      IR_DataBuffer(connectivityBuf, IR_IV_ActiveSlot(p), None, Some(IR_StringConstant(datasetConnectivity)))
    val fields = ListBuffer(
      IR_DataBuffer(velocityBuf, IR_IV_ActiveSlot(u), None, Some(IR_StringConstant(datasetFields.head))),
      IR_DataBuffer(p, IR_IV_ActiveSlot(p), includeGhosts = false, None, Some(IR_StringConstant(datasetFields(1))), canonicalOrder = false),
      IR_DataBuffer(rho, IR_IV_ActiveSlot(rho), includeGhosts = false, None, Some(IR_StringConstant(datasetFields(2))), canonicalOrder = false),
      IR_DataBuffer(mue, IR_IV_ActiveSlot(mue), includeGhosts = false, None, Some(IR_StringConstant(datasetFields(3))), canonicalOrder = false),
      IR_DataBuffer(gamma, IR_IV_ActiveSlot(gamma), includeGhosts = false, None, Some(IR_StringConstant(datasetFields(4))), canonicalOrder = false),
      IR_DataBuffer(phi, IR_IV_ActiveSlot(phi), includeGhosts = false, None, Some(IR_StringConstant(datasetFields(5))), canonicalOrder = false))

    if (constsIncluded) constants ++ fields else fields
  }

  override def writeData(constsIncluded : Boolean) : ListBuffer[IR_Statement] = {
    val stmts = super.writeData(constsIncluded)

    // cleanup
    // TODO remove once temp. buffer IV's work correctly
    if (fmt != "XML") {
      stmts ++= cleanupNodePositions
      stmts += cleanupConnectivity
      stmts += cleanupVelocity
    }

    stmts
  }

}
