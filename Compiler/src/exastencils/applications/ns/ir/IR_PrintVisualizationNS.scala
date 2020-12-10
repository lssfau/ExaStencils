package exastencils.applications.ns.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ArrayAccess
import exastencils.base.ir.IR_ArrayAllocation
import exastencils.base.ir.IR_ArrayFree
import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_Multiplication
import exastencils.base.ir.IR_PointerDatatype
import exastencils.base.ir.IR_Statement
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
import exastencils.visualization.ir.IR_PrintVisualizationQuads

trait IR_PrintVisualizationNS extends IR_PrintVisualizationQuads{
  def numDimsGrid : Int = p.numDimsGrid

  def numCells_x : Int = p.layout.layoutsPerDim(0).numInnerLayers
  def numCells_y : Int = p.layout.layoutsPerDim(1).numInnerLayers
  def numCells_z : Int = if (numDimsGrid > 2) p.layout.layoutsPerDim(2).numInnerLayers else 1
  def numCellsPerFrag : Int = numCells_x * numCells_y * numCells_z

  def dimsPositionsFrag : ListBuffer[IR_IntegerConstant] = ListBuffer(if (numDimsGrid > 2) numCells_z+1 else 1, numCells_y+1, numCells_x+1).map(a => IR_IntegerConstant(a))

  def numFrags : IR_IntegerConstant = Knowledge.domain_numFragmentsTotal

  def fieldnames : ListBuffer[String]
  def numFields : Int = fieldnames.length

  // dataset names for hdf5
  def datasetCoords : ListBuffer[String] = ListBuffer("/constants/X", "/constants/Y", "/constants/Z")
  def datasetConnectivity = "/constants/Connectivity"
  def datasetFields : ListBuffer[String] = fieldnames.map(name => "/fieldData/" + name)

  def u : IR_Field = IR_FieldCollection.getByIdentifier("u", level).get
  def v : IR_Field = IR_FieldCollection.getByIdentifier("v", level).get
  def w : IR_Field = IR_FieldCollection.getByIdentifier("w", level).get
  def p : IR_Field = IR_FieldCollection.getByIdentifier("p", level).get
  def rho : IR_Field = IR_FieldCollection.getByIdentifier("rho", level).get
  def mue : IR_Field = IR_FieldCollection.getByIdentifier("mue", level).get
  def gamma : IR_Field = IR_FieldCollection.getByIdentifier("gamma", level).get
  def phi : IR_Field = IR_FieldCollection.getByIdentifier("phi", level).get

  def someCellField : IR_Field = p

  def velAsVec = Array(meanU, meanV, meanW)

  def meanU : IR_Multiplication = 0.5 * (IR_FieldAccess(u, IR_IV_ActiveSlot(u), IR_LoopOverDimensions.defIt(numDimsGrid))
    + IR_FieldAccess(u, IR_IV_ActiveSlot(u), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(1, 0, 0)))

  def meanV : IR_Multiplication = 0.5 * (IR_FieldAccess(v, IR_IV_ActiveSlot(v), IR_LoopOverDimensions.defIt(numDimsGrid))
    + IR_FieldAccess(v, IR_IV_ActiveSlot(v), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(0, 1, 0)))

  def meanW : IR_Multiplication = 0.5 * (IR_FieldAccess(w, IR_IV_ActiveSlot(w), IR_LoopOverDimensions.defIt(numDimsGrid))
    + IR_FieldAccess(w, IR_IV_ActiveSlot(w), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(0, 0, 1)))

  def velocity_decl = IR_VariableDeclaration(IR_PointerDatatype(IR_IntegerDatatype), "vel")
  def velocity = IR_VariableAccess(velocity_decl)

  def setupVelocity : ListBuffer[IR_Statement] = {
    // init buffer with values of vector field
    var stmts : ListBuffer[IR_Statement] = ListBuffer()

    stmts += velocity_decl

    stmts += IR_ArrayAllocation(velocity, u.resolveBaseDatatype, numDimsGrid * numCellsPerFrag * numFragsPerBlock)

    val initBuffer : ListBuffer[IR_Statement] = ListBuffer() ++ (0 until numDimsGrid).map(d => {
      val linearizedIdx = u.layout.linearizeIndex(IR_LoopOverDimensions.defIt(numDimsGrid))
      IR_Assignment(
        IR_ArrayAccess(velocity, IR_LoopOverFragments.defIt * numCellsPerFrag * numDimsGrid + numDimsGrid * linearizedIdx + d),
        velAsVec(d))
    })

    stmts += IR_LoopOverFragments(
      IR_IfCondition(IR_IV_IsValidForDomain(p.domain.index),
        IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
          IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.layout.idxById("DLB", dim) - Duplicate(p.referenceOffset(dim)) : IR_Expression)),
          IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.layout.idxById("DRE", dim) - Duplicate(p.referenceOffset(dim)) : IR_Expression))),
          initBuffer)))

    stmts
  }

  def cleanupVelocity = IR_ArrayFree(velocity)
}
