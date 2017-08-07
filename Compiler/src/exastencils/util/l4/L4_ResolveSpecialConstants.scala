package exastencils.util.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.logger.Logger

object L4_ResolveSpecialConstants extends DefaultStrategy("Resolve special constants") {
  this += new Transformation("special functions and constants", {
    // math constants
    case access : L4_UnresolvedAccess if "PI" == access.name || "M_PI" == access.name || "Pi" == access.name =>
      L4_RealConstant(math.Pi)

    // access to level information
    case L4_FunctionCall(L4_UnresolvedFunctionReference("levels", Some(L4_SingleLevel(level)), offset), ListBuffer()) =>
      if (offset.isDefined) Logger.warn(s"Found levels function with offset; offset is ignored")
      L4_IntegerConstant(level)

    case L4_FunctionCall(L4_UnresolvedFunctionReference("levelIndex", Some(L4_SingleLevel(level)), offset), ListBuffer()) =>
      if (offset.isDefined) Logger.warn(s"Found levelIndex function with offset; offset is ignored")
      L4_IntegerConstant(level - Knowledge.minLevel)

    case L4_FunctionCall(L4_UnresolvedFunctionReference("levelString", Some(L4_SingleLevel(level)), offset), ListBuffer()) =>
      if (offset.isDefined) Logger.warn(s"Found levelString function with offset; offset is ignored")
      L4_StringConstant(level.toString)
  })
}
