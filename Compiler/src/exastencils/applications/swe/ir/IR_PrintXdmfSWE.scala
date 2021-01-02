package exastencils.applications.swe.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_Index
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
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.ir.IR_IV_ActiveSlot
import exastencils.grid.ir.IR_VF_NodePositionPerDim
import exastencils.io.ir.IR_AccessPattern
import exastencils.io.ir.IR_DataBuffer
import exastencils.io.ir.IR_FileAccess
import exastencils.io.ir.IR_IV_FragmentInfo
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.IR_IV_ConstantsWrittenToFile
import exastencils.visualization.ir.IR_PrintXdmf

// 2D or 3D
// for a fixed number of fragments per block

// TODO test for serial applications

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

  override def writeXdmfGeometry(stream : IR_VariableAccess, global : Boolean) : ListBuffer[IR_Statement] = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    statements += new IR_IfCondition(IR_IV_ConstantsWrittenToFile().isEmpty,
      /* truebody */
      ListBuffer[IR_Statement](
        printXdmfElement(stream, openGeometry("X_Y"))) ++ // nodePositions are not interleaved
        (0 until numDimsGrid).flatMap(d => {
          printXdmfElement(stream, openDataItem(IR_RealDatatype, dimsPositionsFrag :+ dimFrags(global), seekp = getSeekp(global)) : _*) +: (
          if (fmt == "XML") {
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

    statements += new IR_IfCondition(IR_IV_ConstantsWrittenToFile().isEmpty,
      /* truebody */
      ListBuffer[IR_Statement](
        printXdmfElement(stream, openTopology("Triangle", ListBuffer(numCellsPerFrag, dimFrags(global))) : _*),
        printXdmfElement(stream, openDataItem(IR_IntegerDatatype, dimsConnectivityFrag :+ dimFrags(global), seekp = getSeekp(global)) : _*),
        if (fmt == "XML") {
          IR_LoopOverFragments(
            IR_IfCondition(IR_IV_IsValidForDomain(someCellField.domain.index),
              IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DLB", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression)),
                IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DRE", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression))),
                (0 until numDimsGrid).map(dim =>
                  IR_Print(stream, indentData +: separateSequenceAndFilter(connectivityForCell(global = false).take(3 * (dim + 1)).takeRight(3)) :+ IR_Print.newline) : IR_Statement
                ).to[ListBuffer])),
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

    statements ++= fieldnames.zipWithIndex.map { case (fname, fid) =>
      new IR_IfCondition(IR_IV_ConstantsWrittenToFile().isEmpty OrOr fid != fieldnames.indexOf("bath"),
        /* truebody */
        ListBuffer[IR_Statement](
          printXdmfElement(stream, openAttribute(name = fname, tpe = "Scalar", ctr = "Node")),
          printXdmfElement(stream, openDataItem(someCellField.resolveBaseDatatype, dimsPositionsFrag :+ dimFrags(global), seekp = getSeekp(global)) : _*)) ++ (
          if(fmt == "XML") {
            fname match {
              case "bath" => printBath(Some(stream), Some(indentData))
              case "eta"  => printEta(Some(stream), Some(indentData))
              case "u"    => printU(Some(stream), Some(indentData))
              case "v"    => printV(Some(stream), Some(indentData))
              case "order"=> printOrder(Some(stream), Some(indentData))
            }
          } else {
            ListBuffer(printFilename(stream, datasetFields(fid)))
          }) :+ printXdmfElement(stream, closeDataItem) :+
          printXdmfElement(stream, closeAttribute),
        /* falsebody */
        ListBuffer[IR_Statement](
          printXdmfElement(stream, XInclude(href = IR_IV_ConstantsWrittenToFile(), xpath = XPath(s"Attribute[${fid+1}]") : _*) : _*)
        )
      )
    }

    statements
  }

  override def dataBuffers(constsIncluded : Boolean) : ListBuffer[IR_DataBuffer] = {
    // access pattern dependent on reduction mode for blockstructured meshes
    val accessIndices : ListBuffer[IR_Index] = if (Knowledge.swe_nodalReductionPrint)
      ListBuffer(IR_ConstIndex(Array.fill(numDimsGrid)(0)))
    else
      nodeOffsets.map(_.toExpressionIndex)
    val bathAccess = IR_AccessPattern((idx : IR_Index) => IR_FieldAccess(bath, IR_IV_ActiveSlot(bath), idx.toExpressionIndex), accessIndices)

    val constants = nodePosVecAsDataBuffers(accessIndices, datasetCoords.map(s => IR_StringConstant(s))) :+
      IR_DataBuffer(connectivityBuf, IR_IV_ActiveSlot(someCellField), None, Some(IR_StringConstant(datasetConnectivity)), canonicalOrder = false) :+
      IR_DataBuffer(bath, IR_IV_ActiveSlot(bath), includeGhosts = false, Some(bathAccess), Some(IR_StringConstant(datasetFields.head)), canonicalOrder = false)
    val fields = datasetFields.tail.zipWithIndex.map { case (ds, i) =>
      discFieldsAsDataBuffers(discFields(i), ds)
    }

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
      stmts ++= cleanupReducedData
    }

    stmts
  }
}
