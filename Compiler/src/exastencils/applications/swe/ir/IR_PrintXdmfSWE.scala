package exastencils.applications.swe.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_MemberFunctionCall
import exastencils.base.ir.IR_ObjectInstantiation
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_StringDatatype
import exastencils.base.ir.IR_TernaryCondition
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.grid.ir.IR_VF_NodePositionPerDim
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
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
  def datasetCoords = ListBuffer("/constants/X", "/constants/Y")
  def datasetConnectivity = "/constants/Connectivity"
  def datasetFields = "/constants/bath" +: // values don't change -> write once and reference
    fieldnames.drop(1).map(name => "/fieldData/" + name)

  override def stmtsForPreparation = communicateFragmentInfo(
    // in file-per-process, each rank writes its own domain piece individually -> fragOffset = 0
    calculateFragOffset = (ioInterface != "fpp")
  )

  override def writeXdmf() = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // open file and write header
    val stream = newStream
    if(ioInterface == "fpp") {
      val buildStr = IR_VariableAccess(filenamePieceFpp.name, IR_StringDatatype)
      statements += IR_VariableDeclaration(buildStr)
      statements += buildFilenamePiece(stripPath = false, rank = MPI_IV_MpiRank)
      statements += IR_ObjectInstantiation(stream, Duplicate(buildStr))
    } else {
      statements += IR_ObjectInstantiation(stream, Duplicate(filename))
    }
    statements += printXdmfElement(stream, xmlHeader)
    statements += printXdmfElement(stream, openXdmf)
    statements += printXdmfElement(stream, openDomain)

    // write global grid element
    statements ++= writeXdmfGrid(stream, global = (ioInterface != "fpp"))

    // write footer and close file
    statements += printXdmfElement(stream, closeDomain)
    statements += printXdmfElement(stream, closeXdmf)
    statements += IR_MemberFunctionCall(stream, "close")

    statements
  }

  override def writeData() = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // TODO
    fmt match {
      case "Binary" => {
        // distinguish fpp/mpiio
        // ...
      }
      case "HDF"    => {

      }
      case "XML"    => // values already incorporated in xdmf file -> nothing to do
    }

    statements
  }

  /* writes an xdmf "grid" element
      - global = true : for single-shared file approaches (mpiio, hdf5)
      - global = false: for file-per-process. writes a domain piece

      NOTE: in case of XML we write the data directly into the Xdmf file, otherwise we reference the file with the data
  */
  override def writeXdmfGrid(stream : IR_VariableAccess, global : Boolean) = {
    var statements : ListBuffer[IR_Statement] = ListBuffer()

    // KJI order: first dimension shows how many fragments were written
    val dimFrags = if (global) numFrags else numValidFrags

    val indentData = IR_StringConstant("\t\t\t\t\t")

    def printFilename(dataset : String) = IR_Print(stream, ListBuffer(indentData, buildFilenameData) ++
      (if(fmt == "HDF") IR_StringConstant(dataset)::Nil else Nil) :+ IR_Print.newline
    )

    statements += printXdmfElement(stream, openGrid("Grid", "Uniform"))

    /* positions */
    statements += printXdmfElement(stream, openGeometry("X_Y")) // nodePositions are not interleaved
    for (d <- 0 until numDimsGrid) {
      statements += printXdmfElement(stream, openDataItem(IR_RealDatatype, dimFrags +: dimsPositionsFrag, seekp = getSeekp(d, global)) : _*)
      val printValsOrRefFile = if(fmt == "XML") {
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
        ListBuffer(printFilename(datasetCoords(d)))
      }
      statements ++= printValsOrRefFile
      statements += printXdmfElement(stream, closeDataItem)
    }
    statements += printXdmfElement(stream, closeGeometry)

    /* connectivity */
    statements += printXdmfElement(stream, openTopology("Triangle", ListBuffer(dimFrags, numCellsPerFrag)) : _*)
    statements += printXdmfElement(stream, openDataItem(IR_IntegerDatatype, dimFrags +: dimsConnectivityFrag, seekp = getSeekp(2, global)) : _*)
    val printValsOrRefFile = if(fmt == "XML") {
      IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(someCellField.domain.index),
          new IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
            IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DLB", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression)),
            IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DRE", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression))),
            ListBuffer[IR_Statement]() ++ (0 until numDimsGrid).map( dim =>
              IR_Print(stream, indentData +: separateSequenceAndFilter(connectivityForCell.take(3*(dim+1)).takeRight(3)) :+ IR_Print.newline)
            ))),
        IR_Print(stream, IR_Print.flush))
    } else {
      printFilename(datasetConnectivity)
    }
    statements += printValsOrRefFile
    statements += printXdmfElement(stream, closeDataItem)
    statements += printXdmfElement(stream, closeTopology)

    /* fields */
    for(fieldId <- fieldnames.indices) {
      statements += printXdmfElement(stream, openAttribute(name = fieldnames(fieldId), tpe = "Scalar", ctr = "Node"))
      statements += printXdmfElement(stream, openDataItem(someCellField.resolveBaseDatatype, dimFrags +: dimsPositionsFrag, seekp = getSeekp(2 + fieldId, global)) : _*)
      val printValsOrRefFile = if(fmt == "XML") {
        fieldnames(fieldId) match {
          case "bath" => printBath(Some(stream), Some(indentData))
          case "eta"  => printEta(Some(stream), Some(indentData))
          case "u"    => printU(Some(stream), Some(indentData))
          case "v"    => printV(Some(stream), Some(indentData))
          case "order"=> printOrder(Some(stream), Some(indentData))
        }
      } else {
        ListBuffer(printFilename(datasetFields(fieldId)))
      }
      statements ++= printValsOrRefFile
      statements += printXdmfElement(stream, closeDataItem)
      statements += printXdmfElement(stream, closeAttribute)
    }

    statements += printXdmfElement(stream, closeGrid)

    statements
  }

  // ternary condition (depending on constant data reduction) used print the seek pointer for DataItem with index "idx"
  def getSeekp(idx : Int, global : Boolean) = {
    IR_TernaryCondition(constantsWritten,
      seekpOffsets(global, true).take(idx).reduceOption(_ + _).getOrElse(0),
      seekpOffsets(global, false).take(idx).reduceOption(_ + _).getOrElse(0))
  }

  // contains expressions that calculate the seek pointer for each DataItem (used for raw binary files)
  override def seekpOffsets(global : Boolean, constantReduction : Boolean) : ListBuffer[IR_Expression] = {
    val dimFrags = if (global) numFrags else numValidFrags
    val offsetConstants = ListBuffer(
      (dimFrags +: dimsPositionsFrag).reduce(_ * _) * IR_RealDatatype.typicalByteSize, // x coords
      (dimFrags +: dimsPositionsFrag).reduce(_ * _) * IR_RealDatatype.typicalByteSize, // y coords
      (dimFrags +: dimsConnectivityFrag).reduce(_ * _) * IR_IntegerDatatype.typicalByteSize, // connectivity
      (dimFrags +: dimsPositionsFrag).reduce(_ * _) * IR_RealDatatype.typicalByteSize, // bath
    )
    val offsetFields : ListBuffer[IR_Expression] = ListBuffer.fill(numFields-1)((dimFrags +: dimsPositionsFrag).reduce(_ * _) * IR_RealDatatype.typicalByteSize) // eta, u, v, [order]

    if(constantReduction)
      offsetFields
    else
      offsetConstants ++ offsetFields
  }
}
