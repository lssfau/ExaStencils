package exastencils.grid.l4

import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.logger.Logger

/// L4_ResolveGridFunctions

object L4_ResolveGridFunctions extends DefaultStrategy("Resolve grid function accesses") {
  this += new Transformation("Resolve eval and integrate function accesses", {
    case access @ L4_UnresolvedAccess(accessName, _, level, _, _, _) if L4_EvalFunctions.exists(accessName) =>
      if (level.isDefined)
        Logger.warn(s"Found leveled grid function $accessName with level ${ level.get }; level is ignored")
      L4_EvalFunctionAccess(accessName, -1, L4_EvalFunctions.getValue(accessName).get)

    case access @ L4_UnresolvedAccess(accessName, _, level, _, _, _) if L4_IntegrateFunctions.exists(accessName) =>
      if (level.isDefined)
        Logger.warn(s"Found leveled grid function $accessName with level ${ level.get }; level is ignored")
      L4_IntegrateFunctionAccess(accessName, -1, L4_IntegrateFunctions.getValue(accessName).get)
  })

}
