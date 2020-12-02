package exastencils.visualization.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank

trait IR_PrintVisualizationQuads extends IR_PrintVisualization {
  def numFragsPerBlock = Knowledge.domain_numFragmentsPerBlock

  def offsetFragLoop = (MPI_IV_MpiRank * numFragsPerBlock + IR_LoopOverFragments.defIt) * numPointsPerFrag

  def nodeOffsets = ListBuffer(IR_ConstIndex((0 until numDimsGrid).map(_ => 0).toArray))

  def nodalLoopEnd = 0

  def connectivityForCell : Array[IR_Expression] = numDimsGrid match {
    case 2 => Array(
      offsetFragLoop + IR_LoopOverDimensions.defItForDim(0) + 0 + (IR_LoopOverDimensions.defItForDim(1) + 0) * (numCells_x + 1),
      offsetFragLoop + IR_LoopOverDimensions.defItForDim(0) + 1 + (IR_LoopOverDimensions.defItForDim(1) + 0) * (numCells_x + 1),
      offsetFragLoop + IR_LoopOverDimensions.defItForDim(0) + 1 + (IR_LoopOverDimensions.defItForDim(1) + 1) * (numCells_x + 1),
      offsetFragLoop + IR_LoopOverDimensions.defItForDim(0) + 0 + (IR_LoopOverDimensions.defItForDim(1) + 1) * (numCells_x + 1)
    )
    case 3 => Array(
      offsetFragLoop + IR_LoopOverDimensions.defItForDim(0) + 0 + (IR_LoopOverDimensions.defItForDim(1) + 0) * (numCells_x + 1) + (IR_LoopOverDimensions.defItForDim(2) + 0) * (numCells_x + 1) * (numCells_y + 1),
      offsetFragLoop + IR_LoopOverDimensions.defItForDim(0) + 1 + (IR_LoopOverDimensions.defItForDim(1) + 0) * (numCells_x + 1) + (IR_LoopOverDimensions.defItForDim(2) + 0) * (numCells_x + 1) * (numCells_y + 1),
      offsetFragLoop + IR_LoopOverDimensions.defItForDim(0) + 1 + (IR_LoopOverDimensions.defItForDim(1) + 1) * (numCells_x + 1) + (IR_LoopOverDimensions.defItForDim(2) + 0) * (numCells_x + 1) * (numCells_y + 1),
      offsetFragLoop + IR_LoopOverDimensions.defItForDim(0) + 0 + (IR_LoopOverDimensions.defItForDim(1) + 1) * (numCells_x + 1) + (IR_LoopOverDimensions.defItForDim(2) + 0) * (numCells_x + 1) * (numCells_y + 1),
      offsetFragLoop + IR_LoopOverDimensions.defItForDim(0) + 0 + (IR_LoopOverDimensions.defItForDim(1) + 0) * (numCells_x + 1) + (IR_LoopOverDimensions.defItForDim(2) + 1) * (numCells_x + 1) * (numCells_y + 1),
      offsetFragLoop + IR_LoopOverDimensions.defItForDim(0) + 1 + (IR_LoopOverDimensions.defItForDim(1) + 0) * (numCells_x + 1) + (IR_LoopOverDimensions.defItForDim(2) + 1) * (numCells_x + 1) * (numCells_y + 1),
      offsetFragLoop + IR_LoopOverDimensions.defItForDim(0) + 1 + (IR_LoopOverDimensions.defItForDim(1) + 1) * (numCells_x + 1) + (IR_LoopOverDimensions.defItForDim(2) + 1) * (numCells_x + 1) * (numCells_y + 1),
      offsetFragLoop + IR_LoopOverDimensions.defItForDim(0) + 0 + (IR_LoopOverDimensions.defItForDim(1) + 1) * (numCells_x + 1) + (IR_LoopOverDimensions.defItForDim(2) + 1) * (numCells_x + 1) * (numCells_y + 1)
    )
    case _ =>
      Logger.error("Wrong dimensionality in \"IR_PrintVisualizationQuads\".")
  }
}
