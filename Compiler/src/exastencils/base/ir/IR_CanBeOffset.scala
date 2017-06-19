package exastencils.base.ir

import exastencils.datastructures._

/// IR_CanBeOffset

trait IR_CanBeOffset {
  def offsetWith(offset : IR_ConstIndex)
}

/// IR_OffsetAllApplicable

object IR_OffsetAllApplicable extends QuietDefaultStrategy("Offset all applicable expressions") {
  var offset = IR_ConstIndex()

  this += new Transformation("Apply", {
    case toOffset : IR_CanBeOffset =>
      toOffset.offsetWith(offset)
      toOffset
  }, false /* no recursion */)
}
