package exastencils.util.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2._
import exastencils.baseExt.l2.L2_UnresolvedAccess
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.logger.Logger

object L2_ResolveSpecialConstants extends DefaultStrategy("Resolve special constants") {
  this += new Transformation("special functions and constants", {
    // math constants
    case access : L2_UnresolvedAccess if "PI" == access.name || "M_PI" == access.name || "Pi" == access.name =>
      L2_RealConstant(math.Pi)

    case access : L2_UnresolvedAccess if "SQRT2" == access.name || "M_SQRT2" == access.name =>
      L2_RealConstant(math.sqrt(2.0))

    // access to level information
    case L2_FunctionCall(L2_UnresolvedFunctionReference("levels", Some(L2_SingleLevel(level)), offset), ListBuffer()) =>
      if (offset.isDefined) Logger.warn(s"Found levels function with offset; offset is ignored")
      L2_IntegerConstant(level)

    case L2_FunctionCall(L2_UnresolvedFunctionReference("levelIndex", Some(L2_SingleLevel(level)), offset), ListBuffer()) =>
      if (offset.isDefined) Logger.warn(s"Found levelIndex function with offset; offset is ignored")
      L2_IntegerConstant(level - Knowledge.minLevel)

    case L2_FunctionCall(L2_UnresolvedFunctionReference("levelString", Some(L2_SingleLevel(level)), offset), ListBuffer()) =>
      if (offset.isDefined) Logger.warn(s"Found levelString function with offset; offset is ignored")
      L2_StringConstant(level.toString)
  })
}
