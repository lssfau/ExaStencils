package exastencils.util.l1

import exastencils.base.l1._
import exastencils.baseExt.l1.L1_UnresolvedAccess
import exastencils.datastructures._

object L1_ResolveSpecialConstants extends DefaultStrategy("Resolve special constants") {
  this += new Transformation("special functions and constants", {
    // math constants
    case access : L1_UnresolvedAccess if "PI" == access.name || "M_PI" == access.name || "Pi" == access.name =>
      L1_RealConstant(math.Pi)

    case access : L1_UnresolvedAccess if "SQRT2" == access.name || "M_SQRT2" == access.name =>
      L1_RealConstant(math.sqrt(2.0))
  })
}
