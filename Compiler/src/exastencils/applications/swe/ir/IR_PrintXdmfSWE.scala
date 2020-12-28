package exastencils.applications.swe.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.grid.ir.IR_VF_NodePositionPerDim
import exastencils.io.ir.IR_DataBuffer
import exastencils.io.ir.IR_IV_FragmentInfo
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.IR_PrintXdmf

// 2D or 3D
// for a fixed number of fragments per block

// TODO test for serial applications
// TODO reference correct file for constants when using constant data reduction

case class IR_PrintXdmfSWE(
    var filename : IR_Expression,
    level : Int,
    ioMethod : IR_Expression,
    binaryFpp : Boolean) extends IR_PrintXdmf(ioMethod, binaryFpp) with IR_PrintVisualizationSWE with IR_PrintFieldsAsciiSWE {

  // dataset names for hdf5
  def datasetCoords : ListBuffer[String] = ListBuffer("/constants/X", "/constants/Y")
  def datasetConnectivity = "/constants/Connectivity"
  def datasetFields : ListBuffer[String] = "/constants/bath" +: // values don't change -> write once and reference
    fieldnames.drop(1).map(name => "/fieldData/" + name)

  override def stmtsForPreparation : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    // setup frag info
    stmts ++= IR_IV_FragmentInfo.init(
      someCellField.domain.index,
      // in file-per-process, each rank writes its own domain piece individually -> fragOffset = 0
      calculateFragOffset = ioInterface != "fpp"
    )

    // setup buffers
    stmts ++= setupNodePositions
    stmts ++= setupConnectivity(global = ioInterface != "fpp")
    if (Knowledge.swe_nodalReductionPrint) {
      stmts ++= setupReducedData
    }

    stmts
  }

  override def dataBuffers(constants : Boolean) : ListBuffer[IR_DataBuffer] = ???

  override def writeData(constants : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // TODO
    fmt match {
      case "Binary" =>
        // distinguish fpp/mpiio
        // ...
      case "HDF"    =>
        // ...
      case "XML"    => // values already incorporated in xdmf file -> nothing to do
    }

    statements
  }

  override def writeXdmfGeometry(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements += printXdmfElement(stream, openGeometry("X_Y")) // nodePositions are not interleaved
    for (d <- 0 until numDimsGrid) {
      statements += printXdmfElement(stream, openDataItem(IR_RealDatatype, dimsPositionsFrag :+ dimFrags(global), seekp = getSeekp(d, global)) : _*)
      val printValsOrRefFile = if (fmt == "XML") {
        ListBuffer(IR_Print(stream, "std::scientific"),
          IR_LoopOverFragments(
            IR_IfCondition(IR_IV_IsValidForDomain(someCellField.domain.index),
              IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("IB", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression)),
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => nodalLoopEnd + someCellField.layout.idxById("IE", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression))),
                IR_Print(stream, nodeOffsets.flatMap(offset =>
                  ListBuffer(indentData, IR_VF_NodePositionPerDim.access(level, d, IR_LoopOverDimensions.defIt(numDimsGrid) + offset), IR_Print.newline)) : _*))),
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

    statements += printXdmfElement(stream, openTopology("Triangle", ListBuffer(numCellsPerFrag, dimFrags(global))) : _*)
    statements += printXdmfElement(stream, openDataItem(IR_IntegerDatatype, dimsConnectivityFrag :+ dimFrags(global), seekp = getSeekp(2, global)) : _*)
    val printValsOrRefFile = if (fmt == "XML") {
      IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(someCellField.domain.index),
          new IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
            IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DLB", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression)),
            IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DRE", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression))),
            (0 until numDimsGrid).map(dim =>
              IR_Print(stream, indentData +: separateSequenceAndFilter(connectivityForCell(global = false).take(3 * (dim + 1)).takeRight(3)) :+ IR_Print.newline) : IR_Statement
            ).to[ListBuffer])),
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
      statements += printXdmfElement(stream, openAttribute(name = fieldnames(fieldId), tpe = "Scalar", ctr = "Node"))
      statements += printXdmfElement(stream, openDataItem(someCellField.resolveBaseDatatype, dimsPositionsFrag :+ dimFrags(global), seekp = getSeekp(2 + fieldId, global)) : _*)
      val printValsOrRefFile = if(fmt == "XML") {
        fieldnames(fieldId) match {
          case "bath" => printBath(Some(stream), Some(indentData))
          case "eta"  => printEta(Some(stream), Some(indentData))
          case "u"    => printU(Some(stream), Some(indentData))
          case "v"    => printV(Some(stream), Some(indentData))
          case "order"=> printOrder(Some(stream), Some(indentData))
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
}
