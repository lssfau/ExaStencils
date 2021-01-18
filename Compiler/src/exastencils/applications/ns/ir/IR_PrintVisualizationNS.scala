package exastencils.applications.ns.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_Multiplication
import exastencils.base.ir.IR_Statement
import exastencils.baseExt.ir.IR_ExpressionIndexRange
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.ir.IR_FieldCollection
import exastencils.field.ir.IR_IV_ActiveSlot
import exastencils.grid.ir.IR_AtCellCenter
import exastencils.io.ir.IR_IV_TemporaryBuffer
import exastencils.visualization.ir.IR_PrintVisualizationQuads

trait IR_PrintVisualizationNS extends IR_PrintVisualizationQuads{
  def numDimsGrid : Int = p.numDimsGrid

  def numCells_x : Int = p.layout.layoutsPerDim(0).numInnerLayers
  def numCells_y : Int = p.layout.layoutsPerDim(1).numInnerLayers
  def numCells_z : Int = if (numDimsGrid > 2) p.layout.layoutsPerDim(2).numInnerLayers else 1
  def numCellsPerFrag : IR_Expression = numCells_x * numCells_y * numCells_z

  override def dimsPositionsFrag : ListBuffer[IR_Expression] = ListBuffer(numCells_x+1, numCells_y+1, if (numDimsGrid > 2) numCells_z+1 else 1).map(a => IR_IntegerConstant(a))

  def fieldnames : ListBuffer[String]
  def numFields : Int = fieldnames.length

  def u : IR_Field = IR_FieldCollection.getByIdentifier("u", level).get
  def v : IR_Field = IR_FieldCollection.getByIdentifier("v", level).get
  def w : IR_Field = IR_FieldCollection.getByIdentifier("w", level).get
  def p : IR_Field = IR_FieldCollection.getByIdentifier("p", level).get
  def rho : IR_Field = IR_FieldCollection.getByIdentifier("rho", level).get
  def mue : IR_Field = IR_FieldCollection.getByIdentifier("mue", level).get
  def gamma : IR_Field = IR_FieldCollection.getByIdentifier("gamma", level).get
  def phi : IR_Field = IR_FieldCollection.getByIdentifier("phi", level).get

  def someCellField : IR_Field = p

  def velAsVec : Array[IR_Expression] = Array(meanU, meanV) ++ (if (numDimsGrid > 2) Some(meanW) else None)

  def meanU : IR_Multiplication = 0.5 * (IR_FieldAccess(u, IR_IV_ActiveSlot(u), IR_LoopOverDimensions.defIt(numDimsGrid))
    + IR_FieldAccess(u, IR_IV_ActiveSlot(u), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(1, 0, 0)))

  def meanV : IR_Multiplication = 0.5 * (IR_FieldAccess(v, IR_IV_ActiveSlot(v), IR_LoopOverDimensions.defIt(numDimsGrid))
    + IR_FieldAccess(v, IR_IV_ActiveSlot(v), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(0, 1, 0)))

  def meanW : IR_Multiplication = 0.5 * (IR_FieldAccess(w, IR_IV_ActiveSlot(w), IR_LoopOverDimensions.defIt(numDimsGrid))
    + IR_FieldAccess(w, IR_IV_ActiveSlot(w), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(0, 0, 1)))

  // for exodusII: velocity vector must be provided as separate components
  def velocityComponentX = IR_IV_TemporaryBuffer(u.resolveBaseDatatype, IR_AtCellCenter, "velX", domainIndex,
    ListBuffer(numCells_x, numCells_y, numCells_z))
  def velocityComponentY = IR_IV_TemporaryBuffer(v.resolveBaseDatatype, IR_AtCellCenter, "velY", domainIndex,
    ListBuffer(numCells_x, numCells_y, numCells_z))
  def velocityComponentZ : Option[IR_IV_TemporaryBuffer] = if (numDimsGrid > 2)
    Some(IR_IV_TemporaryBuffer(w.resolveBaseDatatype, IR_AtCellCenter, "velZ", domainIndex, ListBuffer(numCells_x, numCells_y, numCells_z)))
  else
    None

  def velocityComponentsAsVec : Array[IR_IV_TemporaryBuffer] = Array(velocityComponentX, velocityComponentY) ++ velocityComponentZ

  def setupVelocityComponents : ListBuffer[IR_Statement] = {
    // init buffer with components of the vector field
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    velocityComponentsAsVec.zipWithIndex.foreach { case (vel, dir) =>
      stmts += vel.getDeclaration()
      stmts += vel.allocateMemory

      def idxRange = IR_ExpressionIndexRange(
        IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.layout.idxById("DLB", dim) - Duplicate(p.referenceOffset(dim)) : IR_Expression)),
        IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.layout.idxById("DRE", dim) - Duplicate(p.referenceOffset(dim)) : IR_Expression)))

      val linearizedIdx = idxRange.linearizeIndex(IR_LoopOverDimensions.defIt(numDimsGrid))
      stmts += IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(p.domain.index),
          IR_LoopOverDimensions(numDimsGrid, idxRange,
            IR_Assignment(
              vel.at(IR_LoopOverFragments.defIt * numCellsPerFrag + linearizedIdx),
              velAsVec(dir)))))
    }

    stmts
  }

  def cleanupVelocityComponents : ListBuffer[IR_Statement] = velocityComponentsAsVec.map(_.getDtor().get).to[ListBuffer]

  def velocityBuf = IR_IV_TemporaryBuffer(u.resolveBaseDatatype, IR_AtCellCenter, "vel", domainIndex,
    ListBuffer(IR_IntegerConstant(numDimsGrid), numCells_x, numCells_y, numCells_z))

  def setupVelocityBuf : ListBuffer[IR_Statement] = {
    // init buffer with values of vector field
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    stmts += velocityBuf.getDeclaration()
    stmts += velocityBuf.allocateMemory

    def idxRange = IR_ExpressionIndexRange(
      IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.layout.idxById("DLB", dim) - Duplicate(p.referenceOffset(dim)) : IR_Expression)),
      IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.layout.idxById("DRE", dim) - Duplicate(p.referenceOffset(dim)) : IR_Expression)))

    val initBuffer : ListBuffer[IR_Statement] = (0 until numDimsGrid).map(d => {
      val linearizedIdx = idxRange.linearizeIndex(IR_LoopOverDimensions.defIt(numDimsGrid))
      IR_Assignment(
        velocityBuf.at(IR_LoopOverFragments.defIt * numCellsPerFrag * numDimsGrid + numDimsGrid * linearizedIdx + d),
        velAsVec(d)) : IR_Statement
    }).to[ListBuffer]

    stmts += IR_LoopOverFragments(
      IR_IfCondition(IR_IV_IsValidForDomain(p.domain.index),
        IR_LoopOverDimensions(numDimsGrid, idxRange,
          initBuffer)))

    stmts
  }

  def cleanupVelocity : IR_Statement = velocityBuf.getDtor().get
}
