package exastencils.grid.l4

import exastencils.datastructures._
import exastencils.datastructures.l4.UnresolvedAccess
import exastencils.logger.Logger

/// L4_ResolveGridFunctions

object L4_ResolveGridFunctions extends DefaultStrategy("Resolve grid function accesses") {
  this += new Transformation("Resolve eval and integrate function accesses", {
    case access @ UnresolvedAccess(accessName, _, level, _, _, _) if L4_EvalFunctions.exists(accessName) =>
      if (level.isDefined)
        Logger.warn(s"Found leveled grid function $accessName with level ${ level.get }; level is ignored")
      L4_EvalFunctionAccess(accessName, L4_EvalFunctions.getValue(accessName).get)

    case access @ UnresolvedAccess(accessName, _, level, _, _, _) if L4_IntegrateFunctions.exists(accessName) =>
      if (level.isDefined)
        Logger.warn(s"Found leveled grid function $accessName with level ${ level.get }; level is ignored")
      L4_IntegrateFunctionAccess(accessName, L4_IntegrateFunctions.getValue(accessName).get)
  })

}
