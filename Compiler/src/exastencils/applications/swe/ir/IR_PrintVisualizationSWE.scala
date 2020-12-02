package exastencils.applications.swe.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_Comment
import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_ForLoop
import exastencils.base.ir.IR_PreIncrement
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.ir.IR_FieldCollection
import exastencils.field.ir.IR_IV_ActiveSlot
import exastencils.logger.Logger
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.IR_PrintVisualizationTriangles

trait IR_PrintVisualizationSWE extends IR_PrintVisualizationTriangles {
  def numDimsGrid = 2

  def numCells_x = etaDiscLower0.layout.layoutsPerDim(0).numInnerLayers
  def numCells_y = etaDiscLower0.layout.layoutsPerDim(1).numInnerLayers
  def numCells_z = 1
  def numCellsPerFrag = 2 * numCells_x * numCells_y * numCells_z
  def numPointsPerFrag = if(Knowledge.swe_nodalReductionPrint) (numCells_x+1)*(numCells_y+1) else 6 * numCells_x * numCells_y

  def bath = IR_FieldCollection.getByIdentifier("bath", level).get

  def etaDiscLower0 = IR_FieldCollection.getByIdentifier("etaDiscLower0", level).get
  def etaDiscLower1 = IR_FieldCollection.getByIdentifier("etaDiscLower1", level).get
  def etaDiscLower2 = IR_FieldCollection.getByIdentifier("etaDiscLower2", level).get
  def etaDiscUpper0 = IR_FieldCollection.getByIdentifier("etaDiscUpper0", level).get
  def etaDiscUpper1 = IR_FieldCollection.getByIdentifier("etaDiscUpper1", level).get
  def etaDiscUpper2 = IR_FieldCollection.getByIdentifier("etaDiscUpper2", level).get

  def uDiscLower0 = IR_FieldCollection.getByIdentifier("uDiscLower0", level).get
  def uDiscLower1 = IR_FieldCollection.getByIdentifier("uDiscLower1", level).get
  def uDiscLower2 = IR_FieldCollection.getByIdentifier("uDiscLower2", level).get
  def uDiscUpper0 = IR_FieldCollection.getByIdentifier("uDiscUpper0", level).get
  def uDiscUpper1 = IR_FieldCollection.getByIdentifier("uDiscUpper1", level).get
  def uDiscUpper2 = IR_FieldCollection.getByIdentifier("uDiscUpper2", level).get

  def vDiscLower0 = IR_FieldCollection.getByIdentifier("vDiscLower0", level).get
  def vDiscLower1 = IR_FieldCollection.getByIdentifier("vDiscLower1", level).get
  def vDiscLower2 = IR_FieldCollection.getByIdentifier("vDiscLower2", level).get
  def vDiscUpper0 = IR_FieldCollection.getByIdentifier("vDiscUpper0", level).get
  def vDiscUpper1 = IR_FieldCollection.getByIdentifier("vDiscUpper1", level).get
  def vDiscUpper2 = IR_FieldCollection.getByIdentifier("vDiscUpper2", level).get

  def etaDisc = ListBuffer(etaDiscLower0, etaDiscLower1, etaDiscLower2, etaDiscUpper0, etaDiscUpper1, etaDiscUpper2)
  def uDisc = ListBuffer(uDiscLower0, uDiscLower1, uDiscLower2, uDiscUpper0, uDiscUpper1, uDiscUpper2)
  def vDisc = ListBuffer(vDiscLower0, vDiscLower1, vDiscLower2, vDiscUpper0, vDiscUpper1, vDiscUpper2)

  def optLocalOrderLower = IR_FieldCollection.getByIdentifier("local_orderLower0", level, suppressError = true)
  def optLocalOrderUpper = IR_FieldCollection.getByIdentifier("local_orderUpper0", level, suppressError = true)

  def someCellField = etaDiscLower0

  def numFields = 4 + (if (optLocalOrderLower.isDefined && optLocalOrderUpper.isDefined) 1 else 0)

  // nodal data reduction
  def reducedCellPrint(stream : IR_VariableAccess, discFields : ListBuffer[IR_Field]) : ListBuffer[IR_Statement] = {

    if (discFields.length != 6) {
      Logger.error("Wrong usage of \"addReducedNodePrint\" in IR_PrintVtkSWE.")
    }

    val low : ListBuffer[IR_Field] = discFields.take(3)
    val upp : ListBuffer[IR_Field] = discFields.takeRight(3)

    def constructLoopForDim(dim : Int, offStart : Int, offEnd : Int, loopStmts : IR_Statement*) = IR_ForLoop(
      IR_VariableDeclaration(IR_LoopOverDimensions.defItForDim(dim), etaDiscLower0.layout.idxById("IB", dim) + offStart - Duplicate(etaDiscLower0.referenceOffset(dim))),
      IR_LoopOverDimensions.defItForDim(dim) < etaDiscLower0.layout.idxById("IE", dim) - offEnd - Duplicate(etaDiscLower0.referenceOffset(dim)),
      IR_PreIncrement(IR_LoopOverDimensions.defItForDim(dim)),
      loopStmts.to[ListBuffer])

    // TODO: comments
    var statementsFragLoop : ListBuffer[IR_Statement] = ListBuffer()

    statementsFragLoop += IR_Comment("lower left corner: vertex contained by 1 triangle")
    statementsFragLoop += IR_Print(stream,
      IR_FieldAccess(low(0), IR_IV_ActiveSlot(low(0)), IR_ExpressionIndex(0, 0)),
      IR_Print.newline)

    statementsFragLoop += IR_Comment("lowermost row w/o corners: vertex contained by 3 triangles")
    statementsFragLoop += constructLoopForDim(dim = 0, offStart = 1, offEnd = 0,
      IR_Print(stream,
        (IR_FieldAccess(low(0), IR_IV_ActiveSlot(low(0)), IR_ExpressionIndex(IR_LoopOverDimensions.defItForDim(0), 0)) +
          IR_FieldAccess(low(1), IR_IV_ActiveSlot(low(1)), IR_ExpressionIndex(IR_LoopOverDimensions.defItForDim(0), 0) + IR_ConstIndex(-1, 0)) +
          IR_FieldAccess(upp(2), IR_IV_ActiveSlot(upp(2)), IR_ExpressionIndex(IR_LoopOverDimensions.defItForDim(0), 0) + IR_ConstIndex(-1, 0))) / 3.0,
        IR_Print.newline))

    statementsFragLoop += IR_Comment("lower right corner: vertex contained by 2 triangles")
    statementsFragLoop += IR_Print(stream,
      (IR_FieldAccess(low(1), IR_IV_ActiveSlot(low(1)), IR_ExpressionIndex(low(1).layout.layoutsPerDim(0).numInnerLayers, 0) + IR_ConstIndex(-1, 0)) +
        IR_FieldAccess(upp(2), IR_IV_ActiveSlot(upp(2)), IR_ExpressionIndex(upp(2).layout.layoutsPerDim(0).numInnerLayers, 0) + IR_ConstIndex(-1, 0))) / 2.0,
      IR_Print.newline)

    statementsFragLoop += IR_Comment("inner rows")
    statementsFragLoop += constructLoopForDim(dim = 1, offStart = 1, offEnd = 0,
      IR_Comment("leftmost column: vertex contained by 3 triangles"),
      IR_Print(stream,
        (IR_FieldAccess(low(0), IR_IV_ActiveSlot(low(0)), IR_ExpressionIndex(0, IR_LoopOverDimensions.defItForDim(1))) +
          IR_FieldAccess(low(2), IR_IV_ActiveSlot(low(2)), IR_ExpressionIndex(0, IR_LoopOverDimensions.defItForDim(1)) + IR_ConstIndex(0, -1)) +
          IR_FieldAccess(upp(1), IR_IV_ActiveSlot(upp(1)), IR_ExpressionIndex(0, IR_LoopOverDimensions.defItForDim(1)) + IR_ConstIndex(0, -1))) / 3.0,
        IR_Print.newline),
      IR_Comment("inner points: vertex contained by 6 triangles"),
      constructLoopForDim(dim = 0, offStart = 1, offEnd = 0,
        IR_Print(stream,
          (IR_FieldAccess(low(0), IR_IV_ActiveSlot(low(0)), IR_LoopOverDimensions.defIt(numDimsGrid)) +
            IR_FieldAccess(low(1), IR_IV_ActiveSlot(low(1)), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(-1, 0)) +
            IR_FieldAccess(upp(2), IR_IV_ActiveSlot(upp(2)), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(-1, 0)) +
            IR_FieldAccess(low(2), IR_IV_ActiveSlot(low(2)), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(0, -1)) +
            IR_FieldAccess(upp(1), IR_IV_ActiveSlot(upp(1)), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(0, -1)) +
            IR_FieldAccess(upp(0), IR_IV_ActiveSlot(upp(0)), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(-1, -1))) / 6.0,
          IR_Print.newline)),
      IR_Comment("rightmost column: vertex contained by 3 triangles"),
      IR_Print(stream,
        (IR_FieldAccess(upp(0), IR_IV_ActiveSlot(upp(0)), IR_ExpressionIndex(upp(0).layout.layoutsPerDim(0).numInnerLayers, IR_LoopOverDimensions.defItForDim(1)) + IR_ConstIndex(-1, -1)) +
          IR_FieldAccess(low(1), IR_IV_ActiveSlot(low(1)), IR_ExpressionIndex(low(1).layout.layoutsPerDim(0).numInnerLayers, IR_LoopOverDimensions.defItForDim(1)) + IR_ConstIndex(-1, 0)) +
          IR_FieldAccess(upp(2), IR_IV_ActiveSlot(upp(2)), IR_ExpressionIndex(upp(2).layout.layoutsPerDim(0).numInnerLayers, IR_LoopOverDimensions.defItForDim(1)) + IR_ConstIndex(-1, 0))) / 3.0,
        IR_Print.newline)
    )

    statementsFragLoop += IR_Comment("upper left corner: vertex contained by 2 triangles")
    statementsFragLoop += IR_Print(stream,
      (IR_FieldAccess(low(2), IR_IV_ActiveSlot(low(2)), IR_ExpressionIndex(0, low(2).layout.layoutsPerDim(1).numInnerLayers) + IR_ConstIndex(0, -1)) +
        IR_FieldAccess(upp(1), IR_IV_ActiveSlot(upp(1)), IR_ExpressionIndex(0, upp(1).layout.layoutsPerDim(1).numInnerLayers) + IR_ConstIndex(0, -1))) / 2.0,
      IR_Print.newline)

    statementsFragLoop += IR_Comment("uppermost row w/o corners: vertex contained by 3 triangles")
    statementsFragLoop += constructLoopForDim(dim = 0, offStart = 1, offEnd = 0,
      IR_Print(stream,
        (IR_FieldAccess(upp(0), IR_IV_ActiveSlot(upp(0)), IR_ExpressionIndex(IR_LoopOverDimensions.defItForDim(0), upp(0).layout.layoutsPerDim(1).numInnerLayers) + IR_ConstIndex(-1, -1)) +
          IR_FieldAccess(low(2), IR_IV_ActiveSlot(low(2)), IR_ExpressionIndex(IR_LoopOverDimensions.defItForDim(0), low(2).layout.layoutsPerDim(1).numInnerLayers) + IR_ConstIndex(0, -1)) +
          IR_FieldAccess(upp(1), IR_IV_ActiveSlot(upp(1)), IR_ExpressionIndex(IR_LoopOverDimensions.defItForDim(0), upp(1).layout.layoutsPerDim(1).numInnerLayers) + IR_ConstIndex(0, -1))) / 3.0,
        IR_Print.newline))

    statementsFragLoop += IR_Comment("upper right corner: vertex contained by 1 triangle")
    statementsFragLoop += IR_Print(stream,
      IR_FieldAccess(upp(0), IR_IV_ActiveSlot(upp(0)), IR_ExpressionIndex((0 until numDimsGrid).map(d => upp(0).layout.layoutsPerDim(d).numInnerLayers).toArray) + IR_ConstIndex(-1, -1)),
      IR_Print.newline)

    statementsFragLoop
  }
}
