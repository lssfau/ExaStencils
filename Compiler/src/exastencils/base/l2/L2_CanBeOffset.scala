package exastencils.base.l2

import exastencils.core.Duplicate
import exastencils.datastructures._

/// L2_CanBeOffset

trait L2_CanBeOffset {
  var offset : Option[L2_ConstIndex]

  def offsetWith(newOffset : L2_ConstIndex) = {
    if (offset.isEmpty)
      offset = Some(newOffset)
    else
      offset = Some(offset.get + newOffset)
  }
}

/// L2_OffsetAllApplicable

object L2_OffsetAllApplicable extends QuietDefaultStrategy("Offset all applicable expressions") {
  var offset = L2_ConstIndex()

  this += new Transformation("Apply", {
    case toOffset : L2_CanBeOffset =>
      toOffset.offsetWith(Duplicate(offset))
      toOffset
  }, false /* no recursion */)
}

/// L2_OffsetAllApplicable

object L2_OffsetAllWithAnnotation extends QuietDefaultStrategy("Offset all applicable expressions with a given annotation") {
  var offset = L2_ConstIndex()
  var requiredAnnot : String = ""

  this += new Transformation("Apply", {
    case toOffset : L2_CanBeOffset if toOffset.hasAnnotation(requiredAnnot) =>
      toOffset.offsetWith(Duplicate(offset))
      toOffset
  }, false /* no recursion */)
}
