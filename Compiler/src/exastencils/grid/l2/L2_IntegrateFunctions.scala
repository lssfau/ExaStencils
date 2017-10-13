package exastencils.grid.l2

import scala.collection.mutable._

import exastencils.base.l2._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.l2.L2_LevelCollector

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

object L2_ResolveIntegrateFunctions extends DefaultStrategy("Resolve grid function references (integrate)") {
  val collector = new L2_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve", {
    case L2_FunctionCall(ref : L2_UnresolvedFunctionReference, args) if L2_IntegrateFunctions.exists(ref.name) =>
      val level = {
        if (ref.level.isDefined) ref.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for reference to ${ ref.name }")
      }

      L2_IntegrateOnGrid(ref.name, level, args, ref.offset)
  })
}
