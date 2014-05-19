package exastencils.primitives

import exastencils.core._
import exastencils.knowledge._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.strategies._

class NeighborInfo(var dir : Array[Int], var index : Int) {
  var label : String = (Knowledge.dimensionality - 1 to 0 by -1).toList.map(i => dimToString(i).toUpperCase + dirToString(dir(i))).mkString("_")
}

case class IndexRange(var begin : MultiIndex = new MultiIndex, var end : MultiIndex = new MultiIndex) {}

object Mapping {
  def resolveMultiIdx(field : Field, index : MultiIndex) : Expression = {
    val ret = Knowledge.dimensionality match {
      case 1 => (index(0))
      case 2 => (index(1) * field.layout(0).total + index(0))
      case 3 => (index(2) * (field.layout(1).total * field.layout(0).total) + index(1) * field.layout(0).total + index(0))
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

