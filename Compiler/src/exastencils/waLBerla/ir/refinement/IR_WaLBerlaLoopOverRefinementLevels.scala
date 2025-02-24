package exastencils.waLBerla.ir.refinement

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.Output
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_UintType

/// IR_WaLBerlaLoopOverRefinementLevels

object IR_WaLBerlaLoopOverRefinementLevels {
  def apply(body : IR_Statement*) = new IR_WaLBerlaLoopOverRefinementLevels(body.to[ListBuffer])
  def defIt = IR_VariableAccess("refLvl", WB_UintType)
}

case class IR_WaLBerlaLoopOverRefinementLevels(var body : ListBuffer[IR_Statement]) extends IR_ScopedStatement with IR_Expandable {
  import IR_WaLBerlaLoopOverRefinementLevels._
  override def expand() : Output[IR_ForLoop] = {
    IR_ForLoop(
      IR_VariableDeclaration(defIt, IR_Cast(WB_UintType, 0)),
      IR_Lower(defIt, Knowledge.waLBerla_refinementLevels),
      IR_PreIncrement(defIt),
      body)
  }
}