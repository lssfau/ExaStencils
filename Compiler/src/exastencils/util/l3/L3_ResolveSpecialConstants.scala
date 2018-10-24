package exastencils.util.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.logger.Logger

object L3_ResolveSpecialConstants extends DefaultStrategy("Resolve special constants") {
  this += new Transformation("special functions and constants", {
    // math constants
    case access : L3_UnresolvedAccess if "PI" == access.name || "M_PI" == access.name || "Pi" == access.name =>
      L3_RealConstant(math.Pi)

    case access : L3_UnresolvedAccess if "SQRT2" == access.name || "M_SQRT2" == access.name =>
      L3_RealConstant(math.sqrt(2.0))

    // access to level information
    case L3_FunctionCall(L3_UnresolvedFunctionReference("levels", Some(L3_SingleLevel(level)), offset), ListBuffer()) =>
      if (offset.isDefined) Logger.warn(s"Found levels function with offset; offset is ignored")
      L3_IntegerConstant(level)

    case L3_FunctionCall(L3_UnresolvedFunctionReference("levelIndex", Some(L3_SingleLevel(level)), offset), ListBuffer()) =>
      if (offset.isDefined) Logger.warn(s"Found levelIndex function with offset; offset is ignored")
      L3_IntegerConstant(level - Knowledge.minLevel)

    case L3_FunctionCall(L3_UnresolvedFunctionReference("levelString", Some(L3_SingleLevel(level)), offset), ListBuffer()) =>
      if (offset.isDefined) Logger.warn(s"Found levelString function with offset; offset is ignored")
      L3_StringConstant(level.toString)
  })
}
