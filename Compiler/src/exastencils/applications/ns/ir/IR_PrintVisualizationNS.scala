package exastencils.applications.ns.ir

import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.config.Knowledge
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.ir.IR_FieldCollection
import exastencils.field.ir.IR_IV_ActiveSlot
import exastencils.visualization.ir.IR_PrintVisualizationQuads

trait IR_PrintVisualizationNS extends IR_PrintVisualizationQuads{
  def numDimsGrid = p.numDimsGrid

  def numCells_x = p.layout.layoutsPerDim(0).numInnerLayers
  def numCells_y = p.layout.layoutsPerDim(1).numInnerLayers
  def numCells_z = if (numDimsGrid > 2) p.layout.layoutsPerDim(2).numInnerLayers else 1
  def numPointsPerFrag = (numCells_x + 1) * (numCells_y + 1) * (if (numDimsGrid > 2) numCells_z + 1 else 1)
  def numFrags = Knowledge.domain_numFragmentsTotal

  def u = IR_FieldCollection.getByIdentifier("u", level).get
  def v = IR_FieldCollection.getByIdentifier("v", level).get
  def w = IR_FieldCollection.getByIdentifier("w", level).get
  def p = IR_FieldCollection.getByIdentifier("p", level).get
  def rho = IR_FieldCollection.getByIdentifier("rho", level).get
  def mue = IR_FieldCollection.getByIdentifier("mue", level).get
  def gamma = IR_FieldCollection.getByIdentifier("gamma", level).get
  def phi = IR_FieldCollection.getByIdentifier("phi", level).get

  def someCellField = p

  def meanU = 0.5 * (IR_FieldAccess(u, IR_IV_ActiveSlot(u), IR_LoopOverDimensions.defIt(numDimsGrid))
    + IR_FieldAccess(u, IR_IV_ActiveSlot(u), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(1, 0, 0)))

  def meanV = 0.5 * (IR_FieldAccess(v, IR_IV_ActiveSlot(v), IR_LoopOverDimensions.defIt(numDimsGrid))
    + IR_FieldAccess(v, IR_IV_ActiveSlot(v), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(0, 1, 0)))

  def meanW = 0.5 * (IR_FieldAccess(w, IR_IV_ActiveSlot(w), IR_LoopOverDimensions.defIt(numDimsGrid))
    + IR_FieldAccess(w, IR_IV_ActiveSlot(w), IR_LoopOverDimensions.defIt(numDimsGrid) + IR_ConstIndex(0, 0, 1)))
}
