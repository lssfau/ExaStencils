package exastencils.util.l3

import exastencils.base.l3.L3_ConstIndex
import exastencils.config.Knowledge

object L3_OffsetAlias {

  def toConstIndex(dir : String, numDims : Int = /* FIXME */ Knowledge.dimensionality) = {
    val offset = L3_ConstIndex(Array.fill(numDims)(0))

    dir match {
      case "east"   => offset(0) = 1
      case "west"   => offset(0) = -1
      case "north"  => offset(1) = 1
      case "south"  => offset(1) = -1
      case "top"    => offset(2) = 1
      case "bottom" => offset(2) = -1
    }

    offset
  }
}
