package exastencils.visualization.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.logger.Logger

trait IR_PrintVisualizationQuads extends IR_PrintVisualization {

  // fields are written fragment-wise -> not canonical
  override def canonicalFileLayout : Boolean = false

  def nodeOffsets : ListBuffer[IR_ConstIndex] = ListBuffer(IR_ConstIndex((0 until numDimsGrid).map(_ => 0).toArray))

  def nodalLoopEnd = 0

  def connectivityForCell(global : Boolean = true) : ListBuffer[IR_Expression] = {
    val offsetFragLoop : IR_Expression = ((if (global) fragmentOffset else IR_IntegerConstant(0)) + IR_LoopOverFragments.defIt) * numPointsPerFrag + connectivityStartIndex

    numDimsGrid match {
      case 2 => ListBuffer(
        offsetFragLoop + IR_LoopOverDimensions.defItForDim(0) + 0 + (IR_LoopOverDimensions.defItForDim(1) + 0) * (numCells_x + 1),
        offsetFragLoop + IR_LoopOverDimensions.defItForDim(0) + 1 + (IR_LoopOverDimensions.defItForDim(1) + 0) * (numCells_x + 1),
        offsetFragLoop + IR_LoopOverDimensions.defItForDim(0) + 1 + (IR_LoopOverDimensions.defItForDim(1) + 1) * (numCells_x + 1),
        offsetFragLoop + IR_LoopOverDimensions.defItForDim(0) + 0 + (IR_LoopOverDimensions.defItForDim(1) + 1) * (numCells_x + 1)
      )
      case 3 => ListBuffer(
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
}
