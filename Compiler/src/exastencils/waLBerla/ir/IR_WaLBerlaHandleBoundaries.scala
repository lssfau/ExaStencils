package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.boundary.ir.IR_HandleBoundariesLike
import exastencils.communication.NeighborInfo
import exastencils.config.Knowledge
import exastencils.logger.Logger
import exastencils.parallelization.ir.IR_ParallelizationInfo

case class IR_WaLBerlaHandleBoundaries(
    var field : IR_WaLBerlaField,
    var slot : IR_Expression,
    var fragIdx : IR_Expression,
    var neighbors : ListBuffer[(NeighborInfo, IR_ExpressionIndexRange)]) extends IR_HandleBoundariesLike {


  def isAtDomainBorder(dirArr : Array[Int]) = {
    val dir = IR_WaLBerlaDirection.getDirnameFromArray(dirArr)
    if (IR_WaLBerlaDirection.isAxisDirection(dir)) {
      val borderName = IR_WaLBerlaDirection.stringFromDirection(dir) match {
        case "N" => "YMax"
        case "S" => "YMin"
        case "E" => "XMax"
        case "W" => "XMin"
        case "T" => "ZMax"
        case "B" => "ZMin"
      }
      IR_MemberFunctionCallArrow(IR_WaLBerlaUtil.getBlocks, s"atDomain${borderName}Border", IR_BooleanDatatype, IR_WaLBerlaLoopOverBlocks.defIt)
    } else {
      Logger.error("Unsupported direction for \"isAtDomainBorder\"")
    }
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
          IR_IfCondition(isAtDomainBorder(neigh._1.dir), loopOverDims) : IR_Statement
        }), IR_ParallelizationInfo(potentiallyParallel = true))
    } else {
      Logger.error("Unsupported")
    }
  }
}
