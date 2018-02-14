package exastencils.base.l4

import exastencils.core.Duplicate
import exastencils.datastructures._

/// L4_CanBeOffset

trait L4_CanBeOffset {
  var offset : Option[L4_ConstIndex]

  def offsetWith(newOffset : L4_ConstIndex) = {
    if (offset.isEmpty)
      offset = Some(newOffset)
    else
      offset = Some(offset.get + newOffset)
  }
}

/// L4_OffsetAllApplicable

object L4_OffsetAllApplicable extends QuietDefaultStrategy("Offset all applicable expressions") {
  var offset = L4_ConstIndex()

  this += new Transformation("Apply", {
    case toOffset : L4_CanBeOffset =>
      toOffset.offsetWith(Duplicate(offset))
      toOffset
  }, false /* no recursion */)
}

/// L4_OffsetAllApplicable

object L4_OffsetAllWithAnnotation extends QuietDefaultStrategy("Offset all applicable expressions with a given annotation") {
  var offset = L4_ConstIndex()
  var requiredAnnot : String = ""

  this += new Transformation("Apply", {
    case toOffset : L4_CanBeOffset if toOffset.hasAnnotation(requiredAnnot) =>
      toOffset.offsetWith(Duplicate(offset))
      toOffset
  }, false /* no recursion */)
}
