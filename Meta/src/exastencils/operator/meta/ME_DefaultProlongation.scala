package exastencils.operator.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_DefaultProlongation extends Generatable {
  override def validLayers() = ListBuffer(L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/operator/|LAYER_LC|/|LAYER_UC|_DefaultProlongation.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.operator.|LAYER_LC|

import scala.collection.mutable.ListBuffer

import exastencils.base.|LAYER_LC|.|LAYER_UC|_ImplicitConversion._
import exastencils.base.|LAYER_LC|._
import exastencils.baseExt.|LAYER_LC|.|LAYER_UC|_FieldIteratorAccess

object |LAYER_UC|_DefaultProlongation {
  type EntryList = ListBuffer[(|LAYER_UC|_ExpressionIndex, |LAYER_UC|_Expression)]

  def generate(numDims : Int, localization : String, interpolation : String) : ListBuffer[|LAYER_UC|_StencilEntry] = {
    var entries : EntryList = ListBuffer((|LAYER_UC|_ExpressionIndex(), |LAYER_UC|_RealConstant(1.0)))

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

    entries.map(entry => |LAYER_UC|_StencilOffsetEntry(entry._1, entry._2))
  }

  def wrapNodeLinear(entries : EntryList, dim : Int) : EntryList = {
    entries.flatMap(entry => ListBuffer(
      (|LAYER_UC|_ExpressionIndex(entry._1.indices :+ (0 : |LAYER_UC|_Expression)), 0.5 * entry._2),
      (|LAYER_UC|_ExpressionIndex(entry._1.indices :+ (|LAYER_UC|_FieldIteratorAccess(dim) Mod 2)), 0.5 * entry._2)
    ))
  }

  def wrapCellLinear(entries : EntryList, dim : Int) : EntryList = {
    entries.flatMap(entry => ListBuffer(
      (|LAYER_UC|_ExpressionIndex(entry._1.indices :+ (0 : |LAYER_UC|_Expression)), entry._2)))
  }
}
"""
  }
}
