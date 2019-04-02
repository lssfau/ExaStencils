package exastencils.util.l4

import exastencils.base.l4.L4_ConstIndex
import exastencils.config.Knowledge

object L4_OffsetAlias {

  def toConstIndex(dir : String, numDims : Int = /* FIXME */ Knowledge.dimensionality) = {
    val offset = L4_ConstIndex(Array.fill(numDims)(0))

    dir match {
      case "center" =>
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
