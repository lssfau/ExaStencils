package exastencils.knowledge

import scala.collection.mutable.ListBuffer
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
    SimplifyStrategy.doUntilDoneStandalone(ret)
    ret
  }

  def resolveMultiIdx(index : MultiIndex, aabb : IndexRange) : Expression = {
    val ret = Knowledge.dimensionality match {
      case 1 => (index(0))
      case 2 => (index(1) * (aabb.end(0) - aabb.begin(0)) + index(0))
      case 3 => (index(2) * ((aabb.end(1) - aabb.begin(1)) * (aabb.end(0) - aabb.begin(0))) + index(1) * (aabb.end(0) - aabb.begin(0)) + index(0))
    }
    SimplifyStrategy.doUntilDoneStandalone(ret)
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

case class InitGeomCoords(var field : Field) extends Statement with Expandable {
  def cpp : String = { return "NOT VALID ; CLASS = InitGeomCoords\n" }

  override def expand : StatementBlock = {
    new StatementBlock(ListBuffer[Statement](
      "const double xPos = " ~ (("(double)" ~ ("x" - field.referenceOffset.index_0) / (field.layout(0).idxDupRightEnd - field.layout(0).idxDupLeftBegin - 1)) * "(curFragment.posEnd.x - curFragment.posBegin.x)" + "curFragment.posBegin.x"),
      if (Knowledge.dimensionality > 1)
        "const double yPos = " ~ (("(double)" ~ ("y" - field.referenceOffset.index_1) / (field.layout(1).idxDupRightEnd - field.layout(1).idxDupLeftBegin - 1)) * "(curFragment.posEnd.y - curFragment.posBegin.y)" + "curFragment.posBegin.y")
      else NullStatement(),
      if (Knowledge.dimensionality > 2)
        "const double zPos = " ~ (("(double)" ~ ("z" - field.referenceOffset.index_2) / (field.layout(2).idxDupRightEnd - field.layout(2).idxDupLeftBegin - 1)) * "(curFragment.posEnd.z - curFragment.posBegin.z)" + "curFragment.posBegin.z")
      else NullStatement()))
  }
}