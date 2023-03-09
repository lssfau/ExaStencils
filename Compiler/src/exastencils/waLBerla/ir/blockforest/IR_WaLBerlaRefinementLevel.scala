package exastencils.waLBerla.ir.blockforest

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_UnduplicatedVariable
import exastencils.datastructures.Node
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation

case class IR_WaLBerlaRefinementLevel() extends IR_WaLBerlaBlockLoopVariable {
  override def resolveName() : String = "refinementLvl"
  override def resolveDatatype() : IR_Datatype = IR_SpecialDatatype("size_t")

  override def getDeclaration() : IR_VariableDeclaration = IR_VariableDeclaration(resolveDatatype(), resolveName(), IR_WaLBerlaBlockForest().getRefinementLvlForIterator())
}
