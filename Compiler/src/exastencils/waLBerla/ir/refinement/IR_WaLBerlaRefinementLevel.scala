package exastencils.waLBerla.ir.refinement

import exastencils.base.ir._
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockLoopVariable

case class IR_WaLBerlaRefinementLevel() extends IR_WaLBerlaBlockLoopVariable {
  override def resolveName() : String = "refinementLvl"
  override def resolveDatatype() : IR_Datatype = IR_SpecialDatatype("size_t")

  override def getDeclaration() : IR_VariableDeclaration = IR_VariableDeclaration(resolveDatatype(), resolveName(), IR_WaLBerlaBlockForest().getRefinementLvlForIterator())
}
