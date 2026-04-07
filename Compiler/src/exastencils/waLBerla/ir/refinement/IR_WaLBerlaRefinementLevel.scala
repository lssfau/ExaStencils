package exastencils.waLBerla.ir.refinement

import exastencils.base.ir._
import exastencils.waLBerla.ir.blockforest._

case class IR_WaLBerlaRefinementLevel(block : IR_WaLBerlaBlock) extends IR_WaLBerlaBlockLoopVariable {
  override def resolveName() : String = "refinementLvl"
  override def resolveDatatype() : IR_Datatype = IR_SpecialDatatype("size_t")

  override def getDeclaration() : IR_VariableDeclaration = IR_VariableDeclaration(resolveDatatype(), resolveName(), IR_WaLBerlaBlockForest().getRefinementLvl(block))
}
