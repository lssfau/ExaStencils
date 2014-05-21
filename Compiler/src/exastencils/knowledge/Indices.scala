package exastencils.knowledge

import exastencils.core._
import exastencils.knowledge._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.strategies._

case class IndexRange(var begin : MultiIndex = new MultiIndex, var end : MultiIndex = new MultiIndex) {}

object Mapping {
  def resolveMultiIdx(layout : Array[FieldLayoutPerDim], index : MultiIndex) : Expression = {
    val ret = Knowledge.dimensionality match {
      case 1 => (index(0))
      case 2 => (index(1) * layout(0).total + index(0))
      case 3 => (index(2) * (layout(1).total * layout(0).total) + index(1) * layout(0).total + index(0))
    }
    do { SimplifyStrategy.applyStandalone(ret) }
    while (SimplifyStrategy.results.last._2.matches > 0) // FIXME: cleaner code
    ret
  }

  def resolveMultiIdx(index : MultiIndex, aabb : IndexRange) : Expression = {
    val ret = Knowledge.dimensionality match {
      case 1 => (index(0))
      case 2 => (index(1) * (aabb.end(0) - aabb.begin(0)) + index(0))
      case 3 => (index(2) * ((aabb.end(1) - aabb.begin(1)) * (aabb.end(0) - aabb.begin(0))) + index(1) * (aabb.end(0) - aabb.begin(0)) + index(0))
    }
    do { SimplifyStrategy.applyStandalone(ret) }
    while (SimplifyStrategy.results.last._2.matches > 0) // FIXME: cleaner code
    ret
  }
}

object dimToString extends (Int => String) {
  // FIXME: this is named inappropriately; move this to a global variable manager as it becomes available
  def apply(dim : Int) : String = {
    return dim match {
      case 0 => "x"
      case 1 => "y"
      case 2 => "z"
      case _ => "UNKNOWN"
    }
  }
}
