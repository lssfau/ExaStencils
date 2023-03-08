package exastencils.waLBerla.ir.blockforest

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_UnduplicatedVariable
import exastencils.datastructures.Node
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation

case class IR_WaLBerlaRefinementLevel() extends IR_UnduplicatedVariable {
  override def resolveName() : String = "refinementLvl"
  override def resolveDatatype() : IR_Datatype = IR_SpecialDatatype("size_t")
}

object IR_WaLBerlaFindAccessWithRefinement extends QuietDefaultStrategy("Find accesses with refinement") {
  var refinementAccess : Option[IR_WaLBerlaRefinementLevel] = None

  override def applyStandalone(node : Node) : Unit = {
    refinementAccess = None
    super.applyStandalone(node)
  }

  this += Transformation("..", {
    case acc : IR_WaLBerlaRefinementLevel =>
      refinementAccess = Some(acc)
      acc
  })
}
