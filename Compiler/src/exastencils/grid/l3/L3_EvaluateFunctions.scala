package exastencils.grid.l3

import scala.collection.mutable._

import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.l3.L3_LevelCollector

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

object L3_ResolveEvaluateFunctions extends DefaultStrategy("Resolve grid function references (evaluate)") {
  val collector = new L3_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve", {
    case L3_FunctionCall(ref : L3_UnresolvedFunctionReference, args) if L3_EvaluateFunctions.exists(ref.name) =>
      val level = {
        if (ref.level.isDefined) ref.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for reference to ${ ref.name }")
      }

      L3_EvaluateOnGrid(ref.name, level, args, ref.offset)
  })
}
