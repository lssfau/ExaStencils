package exastencils.grid.l2

import scala.collection.mutable._

import exastencils.base.l2._
import exastencils.baseExt.l2.L2_UnresolvedAccess
import exastencils.datastructures._
import exastencils.logger.Logger

/// L2_IntegrateFunctions

object L2_IntegrateFunctions {
  val functions = HashSet[String](
    "integrateOverEastFace", "integrateOverWestFace",
    "integrateOverNorthFace", "integrateOverSouthFace",
    "integrateOverTopFace", "integrateOverBottomFace",

    "integrateOverXStaggeredEastFace", "integrateOverXStaggeredNorthFace", "integrateOverXStaggeredTopFace",
    "integrateOverXStaggeredWestFace", "integrateOverXStaggeredSouthFace", "integrateOverXStaggeredBottomFace",

    "integrateOverYStaggeredEastFace", "integrateOverYStaggeredNorthFace", "integrateOverYStaggeredTopFace",
    "integrateOverYStaggeredWestFace", "integrateOverYStaggeredSouthFace", "integrateOverYStaggeredBottomFace",

    "integrateOverZStaggeredEastFace", "integrateOverZStaggeredNorthFace", "integrateOverZStaggeredTopFace",
    "integrateOverZStaggeredWestFace", "integrateOverZStaggeredSouthFace", "integrateOverZStaggeredBottomFace")

  def exists(fctName : String) = functions.contains(fctName)
}

/// L2_ResolveIntegrateFunctions

object L2_ResolveIntegrateFunctions extends DefaultStrategy("Resolve grid function accesses (integrate)") {
  val collector = new L2_LevelCollector
  this.register(collector)

  this += new Transformation("Resolve function accesses", {
    case L2_FunctionCall(access : L2_UnresolvedAccess, args) if L2_IntegrateFunctions.exists(access.name) =>
      val level = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to field ${ access.name }")
      }

      L2_IntegrateOnGrid(access.name, level, args)
  })
}
