package exastencils.solver.ir

import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.knowledge.ir.IR_LeveledKnowledgeObject
import exastencils.optimization.ir.IR_GeneralSimplifyWrapper

/// IR_NamedEquation

case class IR_NamedEquation(
    var name : String, var level : Int,
    var eq : IR_Equation) extends IR_LeveledKnowledgeObject {

  def lhs = eq.lhs
  def rhs = eq.rhs

  def asZeroEquation() : IR_Expression = {
    val zeroEq : IR_Expression = Duplicate(lhs - rhs)
    IR_GeneralSimplifyWrapper.process(zeroEq)
  }
}
