package exastencils.applications.swe.ir

import scala.collection.immutable.ListMap
import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir._
import exastencils.grid.ir.IR_AtNode
import exastencils.grid.ir.IR_VF_NodePositionAsVec
import exastencils.io.ir._
import exastencils.logger.Logger
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.postprocessing.IR_PrintVisualizationTriangles

trait IR_PrintVisualizationSWE extends IR_PrintVisualizationTriangles {
  def numDimsGrid = 2

  def numCells_x : Int = etaDiscLower0.layout.layoutsPerDim(0).numInnerLayers
  def numCells_y : Int = etaDiscLower0.layout.layoutsPerDim(1).numInnerLayers
  def numCells_z = 1
  def numCellsPerFrag : IR_Expression = 2 * numCells_x * numCells_y * numCells_z

  override def dimsPositionsFrag : ListBuffer[IR_Expression] = if (Knowledge.swe_nodalReductionPrint) ListBuffer(numCells_x + 1, numCells_y + 1) else ListBuffer(6, numCells_x, numCells_y)

  // passed as arguments to function
  def discFieldCollection : ListBuffer[ListBuffer[IR_Field]]
  def nodalFieldCollection : ListBuffer[IR_Field]

  def nodalFields : ListMap[String, IR_Field] = ListMap(
    nodalFieldCollection.map(field => field.name -> field) : _*)
  def discFields : ListMap[String, ListBuffer[IR_Field]] = ListMap(
    discFieldCollection.map(discField => getBasenameDiscField(discField) -> discField) : _*)
  def discFieldsReduced : ListMap[String, IR_IV_TemporaryBuffer] = discFields.map { discField =>
    discField._1 -> IR_IV_TemporaryBuffer(discField._2.head.resolveBaseDatatype, IR_AtNode, discField._1, domainIndex, blockwise = true, dimsPositionsFrag)
  }

  def etaDiscLower0 : IR_Field = IR_FieldCollection.getByIdentifier("etaDiscLower0", level).get
  def someCellField : IR_Field = etaDiscLower0

  def fields : ListMap[String, ListBuffer[IR_Field]] = nodalFields.map(field => field._1 -> ListBuffer(field._2)) ++ discFields
  def fieldnames : ListBuffer[String] = fields.keys.to[ListBuffer]
  def numFields : Int = fieldnames.length

  def nodePosVecAsDataBuffers(accessIndices : Option[ListBuffer[IR_Index]], datasets : Option[ListBuffer[IR_Expression]]) : ListBuffer[IR_DataBuffer] = {
    (0 until numDimsGrid).map(dim =>
      IR_DataBuffer(IR_VF_NodePositionAsVec.find(level).associatedField, accessIndices, if (datasets.isDefined) Some(datasets.get(dim)) else None, dim, 0, canonicalFileLayout)
    ).to[ListBuffer]
  }

  // get the common prefix of a disc field and use as name (e.g. etaDiscLower0, etaDiscLower1, ... -> etaDisc)
  def getBasenameDiscField(discField : ListBuffer[IR_Field]) : String = {
    val basename = discField.map(_.name).reduce((a, b) => (a zip b).takeWhile(Function.tupled(_ == _)).map(_._1).mkString)
    if (basename.isEmpty) {
      Logger.error("\"IR_PrintVisualizationSWE:\" Could not extract a common name from disc field components. Components do not belong to the same disc field.")
    }

    basename
  }

  /*
  Store disc components interleaved in buffer
    - in exodus, the individual datasets of each disc field component cannot be joined together as in xdmf
      - this causes the disc components to be written in an interleaved fashion
      - to reduce the number of interleaved accesses, the disc components are joined together in a buffer before writing
    - can be enabled for comparison in Xdmf printers via "swe_interleaveDiscComponentsPrint" flag
  */
  def discFieldBuffers : ListMap[String, IR_IV_TemporaryBuffer] = discFields.map { discField =>
    discField._1 -> IR_IV_TemporaryBuffer(discField._2.head.resolveBaseDatatype, IR_AtNode, discField._1, domainIndex, blockwise = true, dimsPositionsFrag)
  }

  def setupNonReducedDiscData() : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    // join disc components
    discFieldBuffers.values.foreach { tmpBuf =>
      val indexRangeCells = IR_ExpressionIndexRange(
        IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.layout.idxById("IB", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression)),
        IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.layout.idxById("IE", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression)))

      val numAccessesPerCell = nodeOffsets.length // for non-reduced SWE "6"
      val offset = IR_LoopOverFragments.defIt * dimsPositionsFrag.reduce(_ * _) + numAccessesPerCell * indexRangeCells.linearizeIndex(IR_LoopOverDimensions.defIt(numDimsGrid))

      stmts += tmpBuf.allocateMemory
      stmts += IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
          IR_LoopOverDimensions(numDimsGrid, indexRangeCells,
            (0 until numAccessesPerCell).to[ListBuffer].map(idx => {
              IR_Assignment(
                IR_IV_TemporaryBuffer.accessArray(tmpBuf, offset + idx),
                IR_FieldAccess(discFields(tmpBuf.name)(idx), IR_IV_ActiveSlot(someCellField), IR_LoopOverDimensions.defIt(numDimsGrid))) : IR_Statement
            }))))
    }

    stmts
  }

  // access pattern dependent on reduction mode for blockstructured meshes
  def accessIndices : Option[ListBuffer[IR_Index]] = if (Knowledge.swe_nodalReductionPrint)
    None
  else
    Some(nodeOffsets.map(_.toExpressionIndex))

  def nodalAccess(field : IR_Field) = if (Knowledge.swe_nodalReductionPrint)
    IR_RegularAccessPattern(IR_AccessFieldFunction(field, IR_IV_ActiveSlot(field)))
  else
    IR_SWEAccessPattern(IR_AccessFieldFunction(field, IR_IV_ActiveSlot(field)), accessIndices)

  // glue logic for disc fields to be mapped to data buffers
  def discFieldsToDatabuffers(discField : ListBuffer[IR_Field]) : ListBuffer[IR_DataBuffer] = ???

  // calculating an average over a disc field to reduce the amount of data written to file
  def setupReducedData : ListBuffer[IR_Statement] = {
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    // allocate buffers before calculation
    discFieldsReduced.values.foreach { tmpBuf =>
      stmts += tmpBuf.allocateMemory
    }

    // compute averages and store into buffer before writing
    var stmtsFragLoop : ListBuffer[IR_Statement] = ListBuffer()
    discFieldsReduced.values.foreach { tmpBuf =>
      stmtsFragLoop += IR_Comment(s"\n Nodal data reduction for ${ tmpBuf.name } \n")
      stmtsFragLoop ++= reducedCellPrint(IR_VariableAccess(tmpBuf.name, tmpBuf.resolveDatatype()), discFields(tmpBuf.name))
    }

    stmts += IR_LoopOverFragments(
      IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
        stmtsFragLoop
      )
    )

    stmts
  }

  // nodal data reduction
  def reducedCellPrint(buf : IR_VariableAccess, discField : ListBuffer[IR_Field], indentation : Option[IR_StringConstant] = None) : ListBuffer[IR_Statement] = {

    val low : ListBuffer[IR_Field] = discField.take(3)
    val upp : ListBuffer[IR_Field] = discField.takeRight(3)

    if (discField.length != 6) {
      Logger.error("Wrong usage of \"addReducedNodePrint\" in IR_PrintVisualizationSWE.")
    }

    def getIdxNodalLoop(idx : IR_ExpressionIndex) : IR_Expression = IR_ExpressionIndexRange(
      IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.layout.idxById("IB", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression)),
      IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => 1 + etaDiscLower0.layout.idxById("IE", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression))
    ).linearizeIndex(idx)

    def storeOperation(toStore : IR_Expression, idx : IR_ExpressionIndex) : IR_Statement = buf.datatype match {
      case IR_SpecialDatatype("std::ofstream") => IR_Print(buf,
        (if (indentation.isDefined) indentation.get :: Nil else Nil) :+ toStore :+ IR_Print.newline : _*)
      case IR_PointerDatatype(_)               => IR_Assignment(IR_ArrayAccess(buf, numPointsPerFrag * IR_LoopOverFragments.defIt + getIdxNodalLoop(idx)), toStore)
    }

    def constructLoopForDim(dim : Int, offStart : Int, offEnd : Int, loopStmts : IR_Statement*) = IR_ForLoop(
      IR_VariableDeclaration(IR_LoopOverDimensions.defItForDim(dim), etaDiscLower0.layout.idxById("IB", dim) + offStart - Duplicate(etaDiscLower0.referenceOffset(dim))),
      IR_LoopOverDimensions.defItForDim(dim) < etaDiscLower0.layout.idxById("IE", dim) - offEnd - Duplicate(etaDiscLower0.referenceOffset(dim)),
      IR_PreIncrement(IR_LoopOverDimensions.defItForDim(dim)),
      loopStmts.to[ListBuffer])

    var statementsFragLoop : ListBuffer[IR_Statement] = ListBuffer()

    // expression indices for the edges
    val baseIdx_edgeLower = IR_ExpressionIndex(IR_LoopOverDimensions.defItForDim(0), 0)
    val baseIdx_edgeLeft = IR_ExpressionIndex(0, IR_LoopOverDimensions.defItForDim(1))
    val baseIdx_edgeRight = IR_ExpressionIndex(someCellField.layout.layoutsPerDim(0).numInnerLayers, IR_LoopOverDimensions.defItForDim(1))
    val baseIdx_edgeUpper = IR_ExpressionIndex(IR_LoopOverDimensions.defItForDim(0), someCellField.layout.layoutsPerDim(1).numInnerLayers)

    // expression indices for the corners
    val baseIdx_cornerLL = IR_ExpressionIndex(0, 0)
    val baseIdx_cornerLR = IR_ExpressionIndex(low(1).layout.layoutsPerDim(0).numInnerLayers, 0)
    val baseIdx_cornerUL = IR_ExpressionIndex(0, low(2).layout.layoutsPerDim(1).numInnerLayers)
    val baseIdx_cornerUR = IR_ExpressionIndex((0 until numDimsGrid).map(d => someCellField.layout.layoutsPerDim(d).numInnerLayers).toArray)

    statementsFragLoop += IR_Comment("lower left corner: vertex contained by 1 triangle")
    statementsFragLoop += storeOperation(
      IR_FieldAccess(low(0), IR_IV_ActiveSlot(low(0)), baseIdx_cornerLL),
      baseIdx_cornerLL)

    statementsFragLoop += IR_Comment("lowermost row w/o corners: vertex contained by 3 triangles")
    statementsFragLoop += constructLoopForDim(dim = 0, offStart = 1, offEnd = 0,
      storeOperation(
        (IR_FieldAccess(low(0), IR_IV_ActiveSlot(low(0)), baseIdx_edgeLower) +
          IR_FieldAccess(low(1), IR_IV_ActiveSlot(low(1)), baseIdx_edgeLower + IR_ConstIndex(-1, 0)) +
          IR_FieldAccess(upp(2), IR_IV_ActiveSlot(upp(2)), baseIdx_edgeLower + IR_ConstIndex(-1, 0))) / 3.0,
        baseIdx_edgeLower))

    statementsFragLoop += IR_Comment("lower right corner: vertex contained by 2 triangles")
    statementsFragLoop += storeOperation(
      (IR_FieldAccess(low(1), IR_IV_ActiveSlot(low(1)), baseIdx_cornerLR + IR_ConstIndex(-1, 0)) +
        IR_FieldAccess(upp(2), IR_IV_ActiveSlot(upp(2)), baseIdx_cornerLR + IR_ConstIndex(-1, 0))) / 2.0,
      baseIdx_cornerLR)

    statementsFragLoop += IR_Comment("inner rows")
    statementsFragLoop += constructLoopForDim(dim = 1, offStart = 1, offEnd = 0,
      IR_Comment("leftmost column: vertex contained by 3 triangles"),
      storeOperation(
        (IR_FieldAccess(low(0), IR_IV_ActiveSlot(low(0)), baseIdx_edgeLeft) +
          IR_FieldAccess(low(2), IR_IV_ActiveSlot(low(2)), baseIdx_edgeLeft + IR_ConstIndex(0, -1)) +
          IR_FieldAccess(upp(1), IR_IV_ActiveSlot(upp(1)), baseIdx_edgeLeft + IR_ConstIndex(0, -1))) / 3.0,
        baseIdx_edgeLeft),
      IR_Comment("inner points: vertex contained by 6 triangles"),
      constructLoopForDim(dim = 0, offStart = 1, offEnd = 0,
        storeOperation(
          (IR_FieldAccess(low(0), IR_IV_ActiveSlot(low(0)), IR_LoopOverDimensions.defIt(numDimsGrid)) +
            IR_FieldAccess(low(1), IR_IV_ActiveSlot(low(1)), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(-1, 0)) +
            IR_FieldAccess(upp(2), IR_IV_ActiveSlot(upp(2)), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(-1, 0)) +
            IR_FieldAccess(low(2), IR_IV_ActiveSlot(low(2)), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(0, -1)) +
            IR_FieldAccess(upp(1), IR_IV_ActiveSlot(upp(1)), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(0, -1)) +
            IR_FieldAccess(upp(0), IR_IV_ActiveSlot(upp(0)), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(-1, -1))) / 6.0,
          IR_LoopOverDimensions.defIt(numDimsGrid))),
      IR_Comment("rightmost column: vertex contained by 3 triangles"),
      storeOperation(
        (IR_FieldAccess(upp(0), IR_IV_ActiveSlot(upp(0)), baseIdx_edgeRight + IR_ConstIndex(-1, -1)) +
          IR_FieldAccess(low(1), IR_IV_ActiveSlot(low(1)), baseIdx_edgeRight + IR_ConstIndex(-1, 0)) +
          IR_FieldAccess(upp(2), IR_IV_ActiveSlot(upp(2)), baseIdx_edgeRight + IR_ConstIndex(-1, 0))) / 3.0,
        baseIdx_edgeRight)
    )

    statementsFragLoop += IR_Comment("upper left corner: vertex contained by 2 triangles")
    statementsFragLoop += storeOperation(
      (IR_FieldAccess(low(2), IR_IV_ActiveSlot(low(2)), baseIdx_cornerUL + IR_ConstIndex(0, -1)) +
        IR_FieldAccess(upp(1), IR_IV_ActiveSlot(upp(1)), baseIdx_cornerUL + IR_ConstIndex(0, -1))) / 2.0,
      baseIdx_cornerUL)

    statementsFragLoop += IR_Comment("uppermost row w/o corners: vertex contained by 3 triangles")
    statementsFragLoop += constructLoopForDim(dim = 0, offStart = 1, offEnd = 0,
      storeOperation(
        (IR_FieldAccess(upp(0), IR_IV_ActiveSlot(upp(0)), baseIdx_edgeUpper + IR_ConstIndex(-1, -1)) +
          IR_FieldAccess(low(2), IR_IV_ActiveSlot(low(2)), baseIdx_edgeUpper + IR_ConstIndex(0, -1)) +
          IR_FieldAccess(upp(1), IR_IV_ActiveSlot(upp(1)), baseIdx_edgeUpper + IR_ConstIndex(0, -1))) / 3.0,
        baseIdx_edgeUpper))

    statementsFragLoop += IR_Comment("upper right corner: vertex contained by 1 triangle")
    statementsFragLoop += storeOperation(
      IR_FieldAccess(upp(0), IR_IV_ActiveSlot(upp(0)), baseIdx_cornerUR + IR_ConstIndex(-1, -1)),
      baseIdx_cornerUR)

    statementsFragLoop
  }
}
