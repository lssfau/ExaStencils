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
import exastencils.base.ir.IR_StringDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir.IR_IV_ActiveSlot
import exastencils.grid.ir.IR_VF_NodePositionPerDim
import exastencils.io.ir.IR_DataBuffer
import exastencils.io.ir.IR_FileAccess
import exastencils.io.ir.IR_IV_FragmentInfo
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.IR_IV_ConstantsWrittenToFile
import exastencils.visualization.ir.IR_PrintXdmf

case class IR_PrintXdmfNS(
    var filename : IR_Expression,
    level : Int,
    ioMethod : IR_Expression,
    binaryFpp : Boolean) extends IR_PrintXdmf(ioMethod, binaryFpp) with IR_PrintVisualizationNS with IR_PrintFieldAsciiNS {

  def fieldnames : ListBuffer[String] = ListBuffer("vel", "p")

  override def stmtsForPreparation : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    // setup frag info and temp buffers
    if (fmt != "XML") {
      stmts ++= IR_IV_FragmentInfo.init(someCellField.domain.index)
      stmts ++= setupNodePositions
      stmts ++= setupConnectivity(global = ioInterface != "fpp")
      stmts ++= setupVelocity
    }

    stmts
  }

  override def writeXdmfGeometry(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements += new IR_IfCondition(IR_IV_ConstantsWrittenToFile().isEmpty,
      /* truebody */
      ListBuffer[IR_Statement](
        printXdmfElement(stream, openGeometry("X_Y" + (if (numDimsGrid > 2) "_Z" else "")))) ++ // nodePositions are not interleaved
        (0 until numDimsGrid).flatMap(d => {
          printXdmfElement(stream, openDataItem(IR_RealDatatype, dimsPositionsFrag :+ dimFrags(global), seekp = getSeekp(global)) : _*) +: (
          if (fmt == "XML") {
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
          }) :+ printXdmfElement(stream, closeDataItem)
        }) :+ printXdmfElement(stream, closeGeometry),
      /* falsebody */
      ListBuffer[IR_Statement](
        printXdmfElement(stream, XInclude(href = IR_IV_ConstantsWrittenToFile(), xpath = XPath("Geometry") : _*) : _*)
      )
    )

    statements
  }

  override def writeXdmfTopology(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements += IR_IfCondition(IR_IV_ConstantsWrittenToFile().isEmpty,
      /* truebody */
      ListBuffer[IR_Statement](
        printXdmfElement(stream, openTopology(if (numDimsGrid == 2) "Quadrilateral" else "Hexahedron", ListBuffer(numCellsPerFrag, dimFrags(global))) : _*),
        printXdmfElement(stream, openDataItem(IR_IntegerDatatype, dimsConnectivityFrag :+ dimFrags(global), seekp = getSeekp(global)) : _*),
        if (fmt == "XML") {
          IR_LoopOverFragments(
            IR_IfCondition(IR_IV_IsValidForDomain(someCellField.domain.index),
              IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DLB", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression)),
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DRE", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression))),
                IR_Print(stream, indentData +: separateSequenceAndFilter(connectivityForCell(global = false)) :+ IR_Print.newline))),
            IR_Print(stream, IR_Print.flush))
        } else {
          printFilename(stream, datasetConnectivity)
        },
        printXdmfElement(stream, closeDataItem),
        printXdmfElement(stream, closeTopology)),
      /* falsebody */
      ListBuffer[IR_Statement](
        printXdmfElement(stream, XInclude(href = IR_IV_ConstantsWrittenToFile(), xpath = XPath("Topology") : _*) : _*)
      )
    )

    statements
  }

  override def writeXdmfAttributes(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    for (fieldId <- fieldnames.indices) {
      val isVector = fieldnames(fieldId) == "vel"
      val dimsFieldData = IR_IntegerConstant(if (isVector) numDimsGrid else 1)
      val dimsCellData = ListBuffer[IR_Expression](numCells_x, numCells_y, numCells_z)
      statements += printXdmfElement(stream, openAttribute(name = fieldnames(fieldId), tpe = if (isVector) "Vector" else "Scalar", ctr = "Cell"))
      statements += printXdmfElement(stream, openDataItem(someCellField.resolveBaseDatatype, dimsFieldData +: dimsCellData :+ dimFrags(global), seekp = getSeekp(global)) : _*)
      val printValsOrRefFile = if (fmt == "XML") {
        fieldnames(fieldId) match {
          case "vel"   => printVel(Some(stream), Some(indentData))
          case "p"     => printP(Some(stream), Some(indentData))
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

  override def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] = {
    val constants = nodePositionsBuf.indices.to[ListBuffer].map(bufIdx =>
      IR_DataBuffer(nodePositionsBuf(bufIdx), IR_IV_ActiveSlot(p), None, Some(IR_StringConstant(datasetCoords(bufIdx))), canonicalOrder = false)) :+
      IR_DataBuffer(connectivityBuf, IR_IV_ActiveSlot(p), None, Some(IR_StringConstant(datasetConnectivity)), canonicalOrder = false)
    val fields = ListBuffer(
      IR_DataBuffer(velocityBuf, IR_IV_ActiveSlot(u), None, Some(IR_StringConstant(datasetFields.head)), canonicalOrder = false),
      IR_DataBuffer(p, IR_IV_ActiveSlot(p), includeGhosts = false, None, Some(IR_StringConstant(datasetFields(1))),  canonicalOrder = false))

    if (constsIncluded) constants ++ fields else fields
  }

  override def writeData(constsIncluded : Boolean) : ListBuffer[IR_Statement] = {
    val stmts = if (binaryFpp) {
      IR_VariableDeclaration(filenamePieceFpp) +:
        buildFilenamePiece(noPath = false, MPI_IV_MpiRank) +:
        ioHandler(constsIncluded, filenamePieceFpp).statementList
    } else {
      val filenameData = IR_VariableAccess(IR_FileAccess.declareVariable("filenameData"), IR_StringDatatype)
      IR_VariableDeclaration(filenameData, buildFilenameData(noPath = false)) +:
        ioHandler(constsIncluded, filenameData).statementList
    }

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
