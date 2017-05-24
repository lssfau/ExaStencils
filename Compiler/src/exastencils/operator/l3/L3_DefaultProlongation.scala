package exastencils.operator.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.baseExt.l3.L3_FieldIteratorAccess

object L3_DefaultProlongation {
  type EntryList = ListBuffer[(L3_ExpressionIndex, L3_Expression)]

  def generate(numDims : Int, localization : String, interpolation : String) : ListBuffer[L3_StencilEntry] = {
    var entries : EntryList = ListBuffer((L3_ExpressionIndex(), L3_RealConstant(1.0)))

    interpolation match {
      case "linear" =>
        localization.toLowerCase() match {
          case "node"   =>
            for (dim <- 0 until numDims)
              entries = wrapNodeLinear(entries, dim)
          case "cell"   =>
            for (dim <- 0 until numDims)
              entries = wrapCellLinear(entries, dim)
          case "face_x" =>
            for (dim <- 0 until numDims) {
              if (0 == dim)
                entries = wrapNodeLinear(entries, dim)
              else
                entries = wrapCellLinear(entries, dim)
            }
          case "face_y" =>
            for (dim <- 0 until numDims) {
              if (1 == dim)
                entries = wrapNodeLinear(entries, dim)
              else
                entries = wrapCellLinear(entries, dim)
            }
          case "face_z" =>
            for (dim <- 0 until numDims) {
              if (2 == dim)
                entries = wrapNodeLinear(entries, dim)
              else
                entries = wrapCellLinear(entries, dim)
            }
        }
    }

    entries.map(entry => L3_StencilOffsetEntry(entry._1, entry._2))
  }

  def wrapNodeLinear(entries : EntryList, dim : Int) : EntryList = {
    entries.flatMap(entry => ListBuffer(
      (L3_ExpressionIndex(entry._1.indices :+ (0 : L3_Expression)), 0.5 * entry._2),
      (L3_ExpressionIndex(entry._1.indices :+ (L3_FieldIteratorAccess(dim) Mod 2)), 0.5 * entry._2)
    ))
  }

  def wrapCellLinear(entries : EntryList, dim : Int) : EntryList = {
    entries.flatMap(entry => ListBuffer(
      (L3_ExpressionIndex(entry._1.indices :+ (0 : L3_Expression)), entry._2)))
  }
}
