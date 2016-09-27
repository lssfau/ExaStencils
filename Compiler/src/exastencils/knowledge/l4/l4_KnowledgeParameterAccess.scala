package exastencils.knowledge.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.core.Settings
import exastencils.datastructures._
import exastencils.knowledge
import exastencils.logger.Logger

object L4_ResolveKnowledgeParameterAccess extends DefaultStrategy("Resolve accesses to knowledge, settings and platform parameters") {
  def resolveParameterToConstant(obj : AnyRef, ident : String) : L4_Expression = {
    obj.getClass.getMethod(ident).invoke(obj) match {
      case value : java.lang.Integer => L4_IntegerConstant(value.toInt)
      case value : java.lang.Float   => L4_RealConstant(value.toFloat)
      case value : java.lang.Boolean => L4_BooleanConstant(value)
      case value : String            => L4_StringConstant(value) // String is already a subclass of object
      case _                         => Logger.error(s"Trying to access parameter $ident from L4 with unsupported type")
    }
  }

  this += new Transformation("special functions and constants", {
    // get knowledge/settings/platform
    case L4_FunctionCall(access : L4_UnresolvedAccess, ListBuffer(L4_StringConstant(ident))) if "getKnowledge" == access.name =>
      resolveParameterToConstant(knowledge.Knowledge, ident)
    case L4_FunctionCall(access : L4_UnresolvedAccess, ListBuffer(L4_StringConstant(ident))) if "getSetting" == access.name   =>
      resolveParameterToConstant(Settings, ident)
    case L4_FunctionCall(access : L4_UnresolvedAccess, ListBuffer(L4_StringConstant(ident))) if "getPlatform" == access.name  =>
      resolveParameterToConstant(knowledge.Platform, ident)
  })
}