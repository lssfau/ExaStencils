package exastencils.grid.l2

import scala.collection.mutable._

import exastencils.base.l2._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.l2.L2_LevelCollector

/// L2_EvaluateFunctions

object L2_EvaluateFunctions {
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

/// L2_ResolveEvaluateFunctions

object L2_ResolveEvaluateFunctions extends DefaultStrategy("Resolve grid function references (evaluate)") {
  val collector = new L2_LevelCollector
  this.register(collector)

  this += new Transformation("Resolve", {
    case L2_FunctionCall(ref : L2_UnresolvedFunctionReference, args) if L2_EvaluateFunctions.exists(ref.name) =>
      val level = {
        if (ref.level.isDefined) ref.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for reference to ${ ref.name }")
      }

      L2_EvaluateOnGrid(ref.name, level, args, ref.offset)
  })
}
