package exastencils.grid.l3

import scala.collection.mutable._

import exastencils.base.l3._
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.datastructures._
import exastencils.logger.Logger

/// L3_IntegrateFunctions

object L3_IntegrateFunctions {
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

/// L3_ResolveIntegrateFunctions

object L3_ResolveIntegrateFunctions extends DefaultStrategy("Resolve grid function accesses (integrate)") {
  val collector = new L3_LevelCollector
  this.register(collector)

  this += new Transformation("Resolve function accesses", {
    case L3_FunctionCall(access : L3_UnresolvedAccess, args) if L3_IntegrateFunctions.exists(access.name) =>
      val level = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to field ${ access.name }")
      }

      L3_IntegrateOnGrid(access.name, level, args)
  })
}
