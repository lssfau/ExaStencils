package exastencils.operator.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2.L2_ImplicitConversion._
import exastencils.base.l2._
import exastencils.grid.l2.L2_Localization

object L2_DefaultProlongation {
  type EntryList = ListBuffer[(L2_ConstIndex, L2_Expression)]

  def generate(name : String, level : Int, numDims : Int, localization : L2_Localization, interpolation : String) : L2_Stencil = {
    val restriction = L2_DefaultRestriction.generate("dummy", level, numDims, localization, interpolation)

    var prolongation = L2_StencilOps.transpose(restriction)

    // apply scaling factors
    interpolation match {
      case "linear"          => prolongation = L2_StencilOps.scale(prolongation, math.pow(2, numDims))
      case "integral_linear" => // nothing to do
    }

    prolongation.name = name
    prolongation.level = level

    prolongation
  }
}
