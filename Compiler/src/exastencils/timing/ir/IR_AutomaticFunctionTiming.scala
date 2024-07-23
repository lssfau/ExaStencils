package exastencils.timing.ir

import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.config.Knowledge

/// IR_AutomaticTimingCategory

object IR_AutomaticTimingCategory extends Enumeration {
  type Access = Value
  final val ANNOT : String = "TimingCategory"
  final val COMM, WAIT, PACK, UNPACK, APPLYBC, IO = Value

  def categoryEnabled(category : Access) = {
    if (Knowledge.timer_automaticTiming) {
      category match {
        case COMM    => Knowledge.timer_automaticCommTiming
        case WAIT    => Knowledge.timer_automaticWaitTiming
        case PACK    => Knowledge.timer_automaticPackingTiming
        case UNPACK  => Knowledge.timer_automaticUnpackingTiming
        case APPLYBC => Knowledge.timer_automaticBCsTiming
        case IO      => Knowledge.timer_automaticIOTiming
        case _       => false
      }
    } else {
      false
    }
  }

  def categoryLeveled(category : Access): Boolean = {
    category match {
      case COMM | APPLYBC => true
      case IO | WAIT | PACK | UNPACK => false
    }
  }

  exastencils.core.Duplicate.registerConstant(this)
}

/// IR_HasAutomaticTimingCategory

trait IR_HasAutomaticTimingCategory {
  def timingCategory : IR_AutomaticTimingCategory.Access
}

/// IR_IV_AutomaticTimer

case class IR_IV_AutomaticTimer(
    var name : String,
    var timingCategory : IR_AutomaticTimingCategory.Access
) extends IR_PlainTimingIV with IR_HasAutomaticTimingCategory

case class IR_IV_AutomaticLeveledTimer(
    var name : String,
    var timingCategory : IR_AutomaticTimingCategory.Access,
    var level : Int
) extends IR_InternalVariable(false, false, false, true, false) with IR_LeveledTimingIV with IR_HasAutomaticTimingCategory