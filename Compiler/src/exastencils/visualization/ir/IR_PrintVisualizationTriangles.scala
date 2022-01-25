package exastencils.visualization.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge

// 2D only
// for a variable number of fragments per block

trait IR_PrintVisualizationTriangles extends IR_PrintVisualization {

  // fields are written fragment-wise -> not canonical
  override def canonicalFileLayout : Boolean = false

  // offsets used when accessing a field within a loop
  def nodeOffsets : ListBuffer[IR_ConstIndex] = if (Knowledge.swe_nodalReductionPrint)
    ListBuffer(IR_ConstIndex(0, 0))
  else
    ListBuffer(IR_ConstIndex(0, 0), IR_ConstIndex(1, 0), IR_ConstIndex(0, 1), IR_ConstIndex(1, 1), IR_ConstIndex(0, 1), IR_ConstIndex(1, 0))

  /*
    - without the reduction: 6 values for the position- and field data are written per cell
    - with the reduction: position and fields are output per grid node (one value per node) -> extend loop end by 1 to iterate through all nodes instead of cells
  */
  def nodalLoopEnd : Int = if (Knowledge.swe_nodalReductionPrint) 1 else 0

  // offsets for vertex positions: (Lower0, Lower1, Lower2, Upper0, Upper1, Upper2)
  def nodePositionOffsets : Array[Int] = if (Knowledge.swe_nodalReductionPrint) {
    Array(0, 1, numCells_x+1, (numCells_x+1)+1, numCells_x+1, 1) // we're not writing 6 vertices per cell in this case
  } else {
    Array(0, 1, 2, 3, 4, 5)
  }

  def connectivityForCell(global : Boolean = true) : ListBuffer[IR_Expression] = {
    val offsetLoopOverDim = if (Knowledge.swe_nodalReductionPrint) {
      IR_LoopOverDimensions.defItForDim(0) + IR_LoopOverDimensions.defItForDim(1) * (numCells_x+1)
    } else {
      6 * (IR_LoopOverDimensions.defItForDim(0) + IR_LoopOverDimensions.defItForDim(1) * numCells_x)
    }

    // indices into node list to construct cells
    val offsetFragLoop : IR_Expression = //(MPI_IV_MpiRank * Knowledge.domain_numFragmentsPerBlock + IR_LoopOverFragments.defIt) * numPointsPerFrag
      ((if (global) fragmentOffset else IR_IntegerConstant(0)) + IR_LoopOverFragments.defIt) * numPointsPerFrag + connectivityStartIndex

    (0 until 6).map(v => offsetFragLoop + offsetLoopOverDim + nodePositionOffsets(v) : IR_Expression).to[ListBuffer]
  }
}
