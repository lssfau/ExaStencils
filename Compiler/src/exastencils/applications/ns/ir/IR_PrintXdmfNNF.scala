package exastencils.applications.ns.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.core.Duplicate
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.grid.ir.IR_VF_NodePositionPerDim
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.IR_PrintXdmf

case class IR_PrintXdmfNNF(
    var filename : IR_Expression,
    level : Int,
    ioMethod : IR_Expression,
    binaryFpp : Boolean) extends IR_PrintXdmf(ioMethod, binaryFpp) with IR_PrintVisualizationNS with IR_PrintFieldAsciiNS {

  def fieldnames : ListBuffer[String] = ListBuffer("vel", "p", "rho", "mue", "gamma", "phi")

  override def stmtsForPreparation : ListBuffer[IR_Statement] = ListBuffer()

  override def writeXdmfGeometry(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements += printXdmfElement(stream, openGeometry("X_Y" + (if(numDimsGrid > 2) "_Z" else ""))) // nodePositions are not interleaved
    for (d <- 0 until numDimsGrid) {
      statements += printXdmfElement(stream, openDataItem(IR_RealDatatype, dimFrags(global) +: dimsPositionsFrag, seekp = getSeekp(d, global)) : _*)
      val printValsOrRefFile = if (fmt == "XML") {
        ListBuffer(IR_Print(stream, "std::scientific"),
          IR_LoopOverFragments(
            IR_IfCondition(IR_IV_IsValidForDomain(someCellField.domain.index),
              IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DLB", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression)),
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => 1 + someCellField.layout.idxById("DRE", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression))),
                IR_Print(stream, indentData, IR_VF_NodePositionPerDim.access(level, d, IR_LoopOverDimensions.defIt(numDimsGrid)), IR_Print.newline))),
            IR_Print(stream, IR_Print.flush)))
      } else {
        ListBuffer(printFilename(stream, datasetCoords(d)))
      }
      statements ++= printValsOrRefFile
      statements += printXdmfElement(stream, closeDataItem)
    }
    statements += printXdmfElement(stream, closeGeometry)

    statements
  }

  override def writeXdmfTopology(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements += printXdmfElement(stream, openTopology(if(numDimsGrid == 2) "Quadrilateral" else "Hexahedron", ListBuffer(dimFrags(global), numCellsPerFrag)) : _*)
    statements += printXdmfElement(stream, openDataItem(IR_IntegerDatatype, dimFrags(global) +: dimsConnectivityFrag, seekp = getSeekp(numDimsGrid, global)) : _*)
    val printValsOrRefFile = if (fmt == "XML") {
      IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(someCellField.domain.index),
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

    statements
  }

  override def writeXdmfAttributes(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    for(fieldId <- fieldnames.indices) {
      val isVector = fieldnames(fieldId) == "vel"
      val dimsFieldData = IR_IntegerConstant(if(isVector) numDimsGrid else 1)
      val dimsCellData = ListBuffer[IR_Expression](numCells_z, numCells_y, numCells_x)
      statements += printXdmfElement(stream, openAttribute(name = fieldnames(fieldId), tpe = if(isVector) "Vector" else "Scalar", ctr = "Cell"))
      statements += printXdmfElement(stream, openDataItem(someCellField.resolveBaseDatatype, dimFrags(global) +: dimsCellData :+ dimsFieldData, seekp = getSeekp(numDimsGrid + fieldId, global)) : _*)
      val printValsOrRefFile = if(fmt == "XML") {
        fieldnames(fieldId) match {
          case "vel"   => printVel(Some(stream), Some(indentData))
          case "p"     => printP(Some(stream), Some(indentData))
          case "rho"   => printRho(Some(stream), Some(indentData))
          case "mue"   => printMue(Some(stream), Some(indentData))
          case "gamma" => printGamma(Some(stream), Some(indentData))
          case "phi"   => printPhi(Some(stream), Some(indentData))
        }
      } else {
        ListBuffer(printFilename(stream, datasetFields(fieldId)))
      }
      statements ++= printValsOrRefFile
      statements += printXdmfElement(stream, closeDataItem)
      statements += printXdmfElement(stream, closeAttribute)
    }

    statements
  }

  override def writeData : ListBuffer[IR_Statement] = ListBuffer() // TODO

  override def seekpOffsets(global : Boolean, constantReduction : Boolean) : ListBuffer[IR_Expression] = ListBuffer(IR_IntegerConstant(0)) // TODO
}
