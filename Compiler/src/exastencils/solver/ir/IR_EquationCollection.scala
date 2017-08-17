package exastencils.solver.ir

import exastencils.knowledge.ir._

/// IR_EquationCollection

object IR_EquationCollection extends IR_LeveledKnowledgeCollection[IR_NamedEquation] {
  exastencils.core.Duplicate.registerConstant(this)
}
