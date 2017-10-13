package exastencils.base.l3

import exastencils.core.Duplicate
import exastencils.datastructures._

/// L3_CanBeOffset

trait L3_CanBeOffset {
  def offsetWith(offset : L3_ConstIndex)
}

/// L3_OffsetAllApplicable

object L3_OffsetAllApplicable extends QuietDefaultStrategy("Offset all applicable expressions") {
  var offset = L3_ConstIndex()

  this += new Transformation("Apply", {
    case toOffset : L3_CanBeOffset =>
      toOffset.offsetWith(Duplicate(offset))
      toOffset
  }, false /* no recursion */)
}

/// L3_OffsetAllApplicable

object L3_OffsetAllWithAnnotation extends QuietDefaultStrategy("Offset all applicable expressions with a given annotation") {
  var offset = L3_ConstIndex()
  var requiredAnnot : String = ""

  this += new Transformation("Apply", {
    case toOffset : L3_CanBeOffset if toOffset.hasAnnotation(requiredAnnot) =>
      toOffset.offsetWith(Duplicate(offset))
      toOffset
  }, false /* no recursion */)
}
