package exastencils.grid.l4

import scala.collection.mutable._

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.l4.L4_LevelCollector

/// L4_EvaluateFunctions

object L4_EvaluateFunctions {
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

/// L4_ResolveEvaluateFunctions

object L4_ResolveEvaluateFunctions extends DefaultStrategy("Resolve grid function references (evaluate)") {
  val collector = new L4_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve", {
    case L4_FunctionCall(ref : L4_UnresolvedFunctionReference, args) if L4_EvaluateFunctions.exists(ref.name) =>
      val level = {
        if (ref.level.isDefined) ref.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for reference to ${ ref.name }")
      }

      L4_EvaluateOnGrid(ref.name, level, args, ref.offset)
  })
}
