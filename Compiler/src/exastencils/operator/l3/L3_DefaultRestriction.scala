package exastencils.operator.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._

object L3_DefaultRestriction {
  type EntryList = ListBuffer[(L3_ConstIndex, L3_Expression)]

  def generate(numDims : Int, localization : String, interpolation : String) : ListBuffer[L3_StencilEntry] = {
    var entries : EntryList = ListBuffer((L3_ConstIndex(), L3_RealConstant(1.0)))

    interpolation match {
      case "linear" =>
        localization.toLowerCase() match {
          case "node"   =>
            for (dim <- 0 until numDims)
              entries = wrapNodeLinear(entries)
          case "cell"   =>
            for (dim <- 0 until numDims)
              entries = wrapCellLinear(entries)
          case "face_x" =>
            for (dim <- 0 until numDims) {
              if (0 == dim)
                entries = wrapNodeLinear(entries)
              else
                entries = wrapCellLinear(entries)
            }
          case "face_y" =>
            for (dim <- 0 until numDims) {
              if (1 == dim)
                entries = wrapNodeLinear(entries)
              else
                entries = wrapCellLinear(entries)
            }
          case "face_z" =>
            for (dim <- 0 until numDims) {
              if (2 == dim)
                entries = wrapNodeLinear(entries)
              else
                entries = wrapCellLinear(entries)
            }
        }
    }

    entries.map(entry => L3_StencilEntry(entry._1, entry._2))
  }

  def wrapNodeLinear(entries : EntryList) : EntryList = {
    entries.flatMap(entry => ListBuffer(
      (L3_ConstIndex(entry._1.indices :+ -1), 0.25 * entry._2),
      (L3_ConstIndex(entry._1.indices :+ 0), 0.5 * entry._2),
      (L3_ConstIndex(entry._1.indices :+ 1), 0.25 * entry._2)
    ))
  }

  def wrapCellLinear(entries : EntryList) : EntryList = {
    entries.flatMap(entry => ListBuffer(
      (L3_ConstIndex(entry._1.indices :+ 0), 0.5 * entry._2),
      (L3_ConstIndex(entry._1.indices :+ 1), 0.5 * entry._2)))
  }
}
