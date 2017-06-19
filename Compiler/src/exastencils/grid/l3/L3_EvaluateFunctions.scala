package exastencils.grid.l3

import scala.collection.mutable._

import exastencils.base.l3._
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.datastructures._
import exastencils.logger.Logger

/// L3_EvaluateFunctions

object L3_EvaluateFunctions {
  val functions = HashSet[String](
    "evalAtEastFace", "evalAtWestFace",
    "evalAtNorthFace", "evalAtSouthFace",
    "evalAtTopFace", "evalAtBottomFace",

    "evalAtXStaggeredEastFace", "evalAtXStaggeredNorthFace", "evalAtXStaggeredTopFace",
    "evalAtXStaggeredWestFace", "evalAtXStaggeredSouthFace", "evalAtXStaggeredBottomFace",

    "evalAtYStaggeredEastFace", "evalAtYStaggeredNorthFace", "evalAtYStaggeredTopFace",
    "evalAtYStaggeredWestFace", "evalAtYStaggeredSouthFace", "evalAtYStaggeredBottomFace",

    "evalAtZStaggeredEastFace", "evalAtZStaggeredNorthFace", "evalAtZStaggeredTopFace",
    "evalAtZStaggeredWestFace", "evalAtZStaggeredSouthFace", "evalAtZStaggeredBottomFace")

  def exists(fctName : String) = functions.contains(fctName)
}

/// L3_ResolveEvaluateFunctions

object L3_ResolveEvaluateFunctions extends DefaultStrategy("Resolve grid function accesses (evaluate)") {
  val collector = new L3_LevelCollector
  this.register(collector)

  this += new Transformation("Resolve function accesses", {
    case L3_FunctionCall(access : L3_UnresolvedAccess, args) if L3_EvaluateFunctions.exists(access.name) =>
      val level = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to field ${ access.name }")
      }

      L3_EvaluateOnGrid(access.name, level, args)
  })
}
