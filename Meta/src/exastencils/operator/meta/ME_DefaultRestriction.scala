package exastencils.operator.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_DefaultRestriction extends Generatable {
  override def validLayers() = ListBuffer(L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/operator/|LAYER_LC|/|LAYER_UC|_DefaultRestriction.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.operator.|LAYER_LC|

import scala.collection.mutable.ListBuffer

import exastencils.base.|LAYER_LC|.|LAYER_UC|_ImplicitConversion._
import exastencils.base.|LAYER_LC|._

object |LAYER_UC|_DefaultRestriction {
  type EntryList = ListBuffer[(|LAYER_UC|_ConstIndex, |LAYER_UC|_Expression)]

  def generate(numDims : Int, localization : String, interpolation : String) : ListBuffer[|LAYER_UC|_StencilEntry] = {
    var entries : EntryList = ListBuffer((|LAYER_UC|_ConstIndex(), |LAYER_UC|_RealConstant(1.0)))

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

    entries.map(entry => |LAYER_UC|_StencilOffsetEntry(entry._1, entry._2))
  }

  def wrapNodeLinear(entries : EntryList) : EntryList = {
    entries.flatMap(entry => ListBuffer(
      (|LAYER_UC|_ConstIndex(entry._1.indices :+ -1), 0.25 * entry._2),
      (|LAYER_UC|_ConstIndex(entry._1.indices :+ 0), 0.5 * entry._2),
      (|LAYER_UC|_ConstIndex(entry._1.indices :+ 1), 0.25 * entry._2)
    ))
  }

  def wrapCellLinear(entries : EntryList) : EntryList = {
    entries.flatMap(entry => ListBuffer(
      (|LAYER_UC|_ConstIndex(entry._1.indices :+ 0), 0.5 * entry._2),
      (|LAYER_UC|_ConstIndex(entry._1.indices :+ 1), 0.5 * entry._2)))
  }
}
"""
  }
}
