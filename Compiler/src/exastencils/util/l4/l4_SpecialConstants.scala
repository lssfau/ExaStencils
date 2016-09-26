package exastencils.util.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge

object L4_ResolveSpecialConstants extends DefaultStrategy("Resolve special constants") {
  this += new Transformation("special functions and constants", {
    // math constants
    case access : L4_UnresolvedAccess if "PI" == access.name || "M_PI" == access.name || "Pi" == access.name =>
      L4_RealConstant(math.Pi)

    // access to level information
    case L4_FunctionCall(L4_UnresolvedAccess("levels", _, Some(L4_SingleLevel(level)), _, _, _), ListBuffer())      =>
      L4_IntegerConstant(level)
    case L4_FunctionCall(L4_UnresolvedAccess("levelIndex", _, Some(L4_SingleLevel(level)), _, _, _), ListBuffer())  =>
      L4_IntegerConstant(level - knowledge.Knowledge.minLevel)
    case L4_FunctionCall(L4_UnresolvedAccess("levelString", _, Some(L4_SingleLevel(level)), _, _, _), ListBuffer()) =>
      L4_StringConstant(level.toString)
  })
}
