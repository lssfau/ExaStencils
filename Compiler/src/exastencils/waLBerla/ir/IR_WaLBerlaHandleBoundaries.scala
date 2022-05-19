package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.boundary.ir.IR_HandleBoundariesLike
import exastencils.communication.NeighborInfo
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.grid.ir._
import exastencils.logger.Logger
import exastencils.parallelization.ir.IR_ParallelizationInfo
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverBlocks
import exastencils.waLBerla.ir.field._
import exastencils.waLBerla.ir.grid._

case class IR_WaLBerlaHandleBoundaries(
    var field : IR_WaLBerlaField,
    var slot : IR_Expression,
    var fragIdx : IR_Expression,
    var neighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange)]) extends IR_HandleBoundariesLike {

  override def setupFieldUpdate(neigh : NeighborInfo) : ListBuffer[IR_Statement] = {
    // apply local trafo and replace boundaryCoord
    val strat = QuietDefaultStrategy("ResolveBoundaryCoordinates")
    strat += new Transformation("SearchAndReplace", {
      case IR_VirtualFieldAccess(IR_VF_BoundaryPositionPerDim(lvl, domain, dim), index, fragIdx) =>

        def access(vf : IR_WaLBerlaVirtualFieldPerDim) = IR_VirtualFieldAccess(vf, index, fragIdx)

        field.layout.localization match {
          case IR_AtCellCenter =>
            if (0 == neigh.dir(dim)) { // simple projection
              access(IR_WaLBerlaCellCenterPerDim(lvl, domain, dim))
            } else if (neigh.dir(dim) < 0) { // snap to left boundary
              access(IR_WaLBerlaCellCenterPerDim(lvl, domain, dim)) - access(IR_WaLBerlaCellWidthPerDim(lvl, domain, dim)) / 2
            } else { // snap to right boundary
              val center = access(IR_WaLBerlaCellCenterPerDim(lvl, domain, dim))
              val width = access(IR_WaLBerlaCellWidthPerDim(lvl, domain, dim))

              center.index = IR_GridUtil.offsetIndex(center.index, 1, dim)
              width.index = IR_GridUtil.offsetIndex(width.index, 1, dim)

              access(IR_WaLBerlaCellCenterPerDim(lvl, domain, dim)) + access(IR_WaLBerlaCellWidthPerDim(lvl, domain, dim)) / 2
            }
        }
    })

    val bc = Duplicate(field.boundary)
    strat.applyStandalone(IR_Root(bc))

    bc.generateFieldUpdate(field, slot, fragIdx, neigh)
  }

  def constructLoops = {
    val layout = field.layout

    if (Knowledge.comm_onlyAxisNeighbors) {
      IR_WaLBerlaLoopOverBlocks(
        neighbors.map({ neigh =>
          val adaptedIndexRange = IR_ExpressionIndexRange(neigh._2.begin - field.referenceOffset, neigh._2.end - field.referenceOffset)
          // TODO: assumes equal bc's for all components
          adaptedIndexRange.begin.indices ++= (layout.numDimsGrid until numDims).map(dim => 0 : IR_Expression)
          adaptedIndexRange.end.indices ++= (layout.numDimsGrid until numDims).map(dim => layout.idxById("TOT", dim))
          val loopOverDims = IR_LoopOverDimensions(
            numDims,
            adaptedIndexRange,
            setupFieldUpdate(neigh._1))
          loopOverDims.parallelization.potentiallyParallel = true
          loopOverDims.polyOptLevel = 1
          IR_IfCondition(IR_WaLBerlaBlockForest().isAtDomainBorder(neigh._1.dir), loopOverDims) : IR_Statement
        }), IR_ParallelizationInfo(potentiallyParallel = true))
    } else {
      Logger.error("Unsupported")
    }
  }
}
