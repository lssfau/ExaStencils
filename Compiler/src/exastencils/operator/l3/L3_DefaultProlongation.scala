package exastencils.operator.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._

object L3_DefaultProlongation {
  type EntryList = ListBuffer[(L3_ConstIndex, L3_Expression)]

  def generate(name : String, level : Int, numDims : Int, localization : String, interpolation : String) : L3_Stencil = {
    val restriction = L3_DefaultRestriction.generate("dummy", level, numDims, localization, interpolation)

    var prolongation = L3_StencilOps.transpose(restriction)

    // apply scaling factors
    interpolation match {
      case "linear"          => prolongation = L3_StencilOps.scale(prolongation, math.pow(2, numDims))
      case "integral_linear" => // nothing to do
    }
    
    prolongation.name = name
    prolongation.level = level

    prolongation
  }
}
