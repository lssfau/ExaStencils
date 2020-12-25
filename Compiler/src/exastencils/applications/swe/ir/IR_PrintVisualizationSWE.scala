package exastencils.applications.swe.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ArrayAccess
import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_Comment
import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ForLoop
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_PointerDatatype
import exastencils.base.ir.IR_PreIncrement
import exastencils.base.ir.IR_SpecialDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.ir.IR_FieldCollection
import exastencils.field.ir.IR_IV_ActiveSlot
import exastencils.io.ir.IR_IV_TemporaryBuffer
import exastencils.logger.Logger
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.IR_PrintVisualizationTriangles

trait IR_PrintVisualizationSWE extends IR_PrintVisualizationTriangles {
  def numDimsGrid = 2

  def numCells_x : Int = etaDiscLower0.layout.layoutsPerDim(0).numInnerLayers
  def numCells_y : Int = etaDiscLower0.layout.layoutsPerDim(1).numInnerLayers
  def numCells_z = 1
  def numCellsPerFrag : Int = 2 * numCells_x * numCells_y * numCells_z

  def dimsPositionsFrag : ListBuffer[IR_IntegerConstant] = if(Knowledge.swe_nodalReductionPrint) ListBuffer(numCells_x+1, numCells_y+1) else ListBuffer(6, numCells_x, numCells_y)

  def bath : IR_Field = IR_FieldCollection.getByIdentifier("bath", level).get

  def etaDiscLower0 : IR_Field = IR_FieldCollection.getByIdentifier("etaDiscLower0", level).get
  def etaDiscLower1 : IR_Field = IR_FieldCollection.getByIdentifier("etaDiscLower1", level).get
  def etaDiscLower2 : IR_Field = IR_FieldCollection.getByIdentifier("etaDiscLower2", level).get
  def etaDiscUpper0 : IR_Field = IR_FieldCollection.getByIdentifier("etaDiscUpper0", level).get
  def etaDiscUpper1 : IR_Field = IR_FieldCollection.getByIdentifier("etaDiscUpper1", level).get
  def etaDiscUpper2 : IR_Field = IR_FieldCollection.getByIdentifier("etaDiscUpper2", level).get

  def uDiscLower0 : IR_Field = IR_FieldCollection.getByIdentifier("uDiscLower0", level).get
  def uDiscLower1 : IR_Field = IR_FieldCollection.getByIdentifier("uDiscLower1", level).get
  def uDiscLower2 : IR_Field = IR_FieldCollection.getByIdentifier("uDiscLower2", level).get
  def uDiscUpper0 : IR_Field = IR_FieldCollection.getByIdentifier("uDiscUpper0", level).get
  def uDiscUpper1 : IR_Field = IR_FieldCollection.getByIdentifier("uDiscUpper1", level).get
  def uDiscUpper2 : IR_Field = IR_FieldCollection.getByIdentifier("uDiscUpper2", level).get

  def vDiscLower0 : IR_Field = IR_FieldCollection.getByIdentifier("vDiscLower0", level).get
  def vDiscLower1 : IR_Field = IR_FieldCollection.getByIdentifier("vDiscLower1", level).get
  def vDiscLower2 : IR_Field = IR_FieldCollection.getByIdentifier("vDiscLower2", level).get
  def vDiscUpper0 : IR_Field = IR_FieldCollection.getByIdentifier("vDiscUpper0", level).get
  def vDiscUpper1 : IR_Field = IR_FieldCollection.getByIdentifier("vDiscUpper1", level).get
  def vDiscUpper2 : IR_Field = IR_FieldCollection.getByIdentifier("vDiscUpper2", level).get

  def etaDisc : ListBuffer[IR_Field] = ListBuffer(etaDiscLower0, etaDiscLower1, etaDiscLower2, etaDiscUpper0, etaDiscUpper1, etaDiscUpper2)
  def uDisc : ListBuffer[IR_Field] = ListBuffer(uDiscLower0, uDiscLower1, uDiscLower2, uDiscUpper0, uDiscUpper1, uDiscUpper2)
  def vDisc : ListBuffer[IR_Field] = ListBuffer(vDiscLower0, vDiscLower1, vDiscLower2, vDiscUpper0, vDiscUpper1, vDiscUpper2)

  def optLocalOrderLower : Option[IR_Field] = IR_FieldCollection.getByIdentifier("local_orderLower0", level, suppressError = true)
  def optLocalOrderUpper : Option[IR_Field] = IR_FieldCollection.getByIdentifier("local_orderUpper0", level, suppressError = true)

  def orderDisc : Option[ListBuffer[IR_Field]] = if(optLocalOrderLower.isDefined && optLocalOrderUpper.isDefined)
    Some((0 until 3).map(_ => optLocalOrderLower.get).to[ListBuffer] ++ (0 until 3).map(_ => optLocalOrderUpper.get))
  else
    None

  def someCellField : IR_Field = etaDiscLower0

  def fieldnames : ListBuffer[String] = ListBuffer("bath", "eta", "u", "v") ++ (if(optLocalOrderLower.isDefined && optLocalOrderUpper.isDefined) "order"::Nil else Nil)
  def numFields : Int = fieldnames.length

  def setupReducedData : ListBuffer[IR_Statement] = {
    val eta = IR_IV_TemporaryBuffer(etaDiscLower0.resolveBaseDatatype, "etaReduced", etaDiscLower0.domain.index, ListBuffer() ++ dimsPositionsFrag)
    val u = IR_IV_TemporaryBuffer(uDiscLower0.resolveBaseDatatype, "uReduced", uDiscLower0.domain.index, ListBuffer() ++ dimsPositionsFrag)
    val v = IR_IV_TemporaryBuffer(vDiscLower0.resolveBaseDatatype, "vReduced", vDiscLower0.domain.index, ListBuffer() ++ dimsPositionsFrag)
    val order = if(orderDisc.isDefined)
      Some(IR_IV_TemporaryBuffer(optLocalOrderLower.get.resolveBaseDatatype, "orderReduced", optLocalOrderLower.get.domain.index, ListBuffer() ++ dimsPositionsFrag))
    else
      None

    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    // allocate buffers before calculations
    stmts += eta.allocateMemory
    stmts += u.allocateMemory
    stmts += v.allocateMemory
    if (orderDisc.isDefined)
      stmts += order.get.allocateMemory

    // compute averages and store into buffer before writing
    var stmtsFragLoop : ListBuffer[IR_Statement] = ListBuffer()
    stmtsFragLoop += IR_Comment("\\n Nodal data reduction for eta \\n")
    stmtsFragLoop ++= reducedCellPrint(IR_VariableAccess(eta.name, eta.resolveDatatype()), etaDisc)
    stmtsFragLoop += IR_Comment("\\n Nodal data reduction for u \\n")
    stmtsFragLoop ++= reducedCellPrint(IR_VariableAccess(u.name, u.resolveDatatype()), uDisc)
    stmtsFragLoop += IR_Comment("\\n Nodal data reduction for v \\n")
    stmtsFragLoop ++= reducedCellPrint(IR_VariableAccess(v.name, v.resolveDatatype()), vDisc)
    if (orderDisc.isDefined) {
      stmtsFragLoop += IR_Comment("\\n Nodal data reduction for order \\n")
      stmtsFragLoop ++= reducedCellPrint(IR_VariableAccess(order.get.name, order.get.resolveDatatype()), orderDisc.get)
    }

    stmts += IR_LoopOverFragments(
      IR_IfCondition(IR_IV_IsValidForDomain(someCellField.domain.index),
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
      Logger.error("Wrong usage of \"addReducedNodePrint\" in IR_PrintVtkSWE.")
    }

    def getIdxNodalLoop(idx : IR_ExpressionIndex) : IR_Expression = IR_ExpressionIndexRange(
      IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.layout.idxById("IB", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression)),
      IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => 1 + etaDiscLower0.layout.idxById("IE", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression))
    ).linearizeIndex(idx)

    def storeOperation(toStore : IR_Expression, idx : IR_ExpressionIndex) : IR_Statement = buf.datatype match {
      case IR_SpecialDatatype("std::ofstream") => IR_Print(buf,
        (if(indentation.isDefined) indentation.get :: Nil else Nil) :+ toStore :+ IR_Print.newline : _*)
      case IR_PointerDatatype(_) => IR_Assignment(IR_ArrayAccess(buf, numPointsPerFrag * IR_LoopOverFragments.defIt + getIdxNodalLoop(idx)), toStore)
    }

    def constructLoopForDim(dim : Int, offStart : Int, offEnd : Int, loopStmts : IR_Statement*) = IR_ForLoop(
      IR_VariableDeclaration(IR_LoopOverDimensions.defItForDim(dim), etaDiscLower0.layout.idxById("IB", dim) + offStart - Duplicate(etaDiscLower0.referenceOffset(dim))),
      IR_LoopOverDimensions.defItForDim(dim) < etaDiscLower0.layout.idxById("IE", dim) - offEnd - Duplicate(etaDiscLower0.referenceOffset(dim)),
      IR_PreIncrement(IR_LoopOverDimensions.defItForDim(dim)),
      loopStmts.to[ListBuffer])

    var statementsFragLoop : ListBuffer[IR_Statement] = ListBuffer()

    // expression indices for the edges
    val baseIdx_edgeLower = IR_ExpressionIndex(IR_LoopOverDimensions.defItForDim(0), 0)
    val baseIdx_edgeLeft  = IR_ExpressionIndex(0, IR_LoopOverDimensions.defItForDim(1))
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
