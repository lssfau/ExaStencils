package exastencils.strategies.ir

import exastencils.base.ir.{IR_AdditionExpression, IR_Expression, IR_IntegerConstant}
import exastencils.core.Duplicate
import exastencils.datastructures.{Node, QuietDefaultStrategy, Transformation}
import exastencils.field.ir.{IR_IV_ActiveSlot, IR_MultiDimFieldAccess, IR_SlotAccess}
import exastencils.logger.Logger
import exastencils.strategies.SimplifyStrategy

import scala.collection.mutable

// FIXME Integrate this into the framework. Ideally this should represent a simple read-only transformation.
// No need for all the transaction machinery.
/** Only implements applyStandalone(node: Node) : Unit. */
abstract  class StandaloneStrategy extends QuietDefaultStrategy("Some StandaloneStrategy") {
  override final def apply(applyAtNode : Option[Node] = None) : Unit = ???
  override final def applyStandalone[T](nodes : mutable.Buffer[T]) : Unit = ???
  override final def applyStandalone[T](nodes : Seq[T]) : Seq[T] = ???
}


/** Read-only transformation that extracts a [[exastencils.field.ir.IR_SlotAccess]] as an integer constant.
  *
  * This is useful for identifying [[IR_MultiDimFieldAccess]]es as variables in pseudo-code.
  */
object SlotAccessAsConst {
  def apply(mdfa:IR_MultiDimFieldAccess) : Option[IR_IntegerConstant] = {
    val strategy = new SlotAccessAsConst()
    strategy.applyStandalone(mdfa)
    strategy.slotId
  }
}

class SlotAccessAsConst extends StandaloneStrategy {
  var slotId : Option[IR_IntegerConstant] = None

  private var hasSlotAccess = false

  override def applyStandalone(node : Node) : Unit = {
    val mdfa = node.asInstanceOf[IR_MultiDimFieldAccess]
    val clone = Duplicate(mdfa)

    super.applyStandalone(clone)

    if(hasSlotAccess) {
      SimplifyStrategy.doUntilDoneStandalone(clone)
      clone.fieldSelection.slot match {
        case slot : IR_IntegerConstant =>
          slotId = Some(slot)
        case _ => Logger.warn("SlotAccessAsConst: IR_SlotAccess was not reduced to IR_IntegerConstant.")
      }
    }
  }

  // transform IR_SlotAccess(...IR_IV_ActiveSlot(...)...)
  // to some_basic_math(...0...)
  this += new Transformation("IR_SlotAccess to simple expression", {
    case sa : IR_SlotAccess =>
      hasSlotAccess = true
      sa.expandSpecial

    case as :IR_IV_ActiveSlot => IR_IntegerConstant(0)
  })
}
