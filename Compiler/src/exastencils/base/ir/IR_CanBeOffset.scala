package exastencils.base.ir

import exastencils.core.Duplicate
import exastencils.datastructures._

/// IR_CanBeOffset

trait IR_CanBeOffset {
  var offset : Option[IR_ConstIndex]

  def offsetWith(newOffset : IR_ConstIndex) = {
    if (offset.isEmpty)
      offset = Some(newOffset)
    else
      offset = Some(offset.get + newOffset)
  }
}

/// IR_OffsetAllApplicable

object IR_OffsetAllApplicable extends QuietDefaultStrategy("Offset all applicable expressions") {
  var offset = IR_ConstIndex()

  this += new Transformation("Apply", {
    case toOffset : IR_CanBeOffset =>
      toOffset.offsetWith(Duplicate(offset))
      toOffset
  }, false /* no recursion */)
}

/// IR_OffsetAllApplicable

object IR_OffsetAllWithAnnotation extends QuietDefaultStrategy("Offset all applicable expressions with a given annotation") {
  var offset = IR_ConstIndex()
  var requiredAnnot : String = ""

  this += new Transformation("Apply", {
    case toOffset : IR_CanBeOffset if toOffset.hasAnnotation(requiredAnnot) =>
      toOffset.offsetWith(Duplicate(offset))
      toOffset
  }, false /* no recursion */)
}
