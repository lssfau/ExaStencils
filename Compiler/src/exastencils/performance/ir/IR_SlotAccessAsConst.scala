package exastencils.performance.ir

import exastencils.base.ir.IR_IntegerConstant
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_GeneralSimplify

/// IR_SlotAccessAsConst

/** Read-only transformation that extracts a [[exastencils.field.ir.IR_SlotAccess]] as an integer constant.
  *
  * This is useful for identifying [[IR_MultiDimFieldAccess]]es as variables in pseudo-code.
  */
object IR_SlotAccessAsConst {
  def apply(mdfa : IR_MultiDimFieldAccess) : Option[IR_IntegerConstant] = {
    val strategy = new IR_SlotAccessAsConst()
    strategy.applyStandalone(mdfa)
    strategy.slotId
  }
}

class IR_SlotAccessAsConst extends StandaloneStrategy {
  var slotId : Option[IR_IntegerConstant] = None

  private var hasSlotAccess = false

  override def applyStandalone(node : Node) : Unit = {
    val mdfa = node.asInstanceOf[IR_MultiDimFieldAccess]
    val clone = Duplicate(mdfa)

    super.applyStandalone(clone)

    if (hasSlotAccess) {
      IR_GeneralSimplify.doUntilDoneStandalone(clone)
      clone.slot match {
        case slot : IR_IntegerConstant =>
          slotId = Some(slot)
        case _                         => Logger.warn("SlotAccessAsConst: IR_SlotAccess was not reduced to IR_IntegerConstant.")
      }
    }
  }

  // transform IR_SlotAccess(...IR_IV_ActiveSlot(...)...)
  // to some_basic_math(...0...)
  this += new Transformation("IR_SlotAccess to simple expression", {
    case sa : IR_SlotAccess =>
      hasSlotAccess = true
      sa.expandSpecial()

    case as : IR_IV_ActiveSlot => IR_IntegerConstant(0)
  })
}
