package exastencils.visualization.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge

trait IR_PrintVisualizationTriangles extends IR_PrintVisualization {
  // offsets used when accessing a field within a loop
  def nodeOffsets = if(Knowledge.swe_nodalReductionPrint)
    ListBuffer(IR_ConstIndex(0, 0))
  else
    ListBuffer(IR_ConstIndex(0, 0), IR_ConstIndex(1, 0), IR_ConstIndex(0, 1), IR_ConstIndex(1, 1), IR_ConstIndex(0, 1), IR_ConstIndex(1, 0))

  def numFrags = IR_VariableAccess("totalNumFrags", IR_IntegerDatatype) // Knowledge.domain_numFragmentsTotal
  def numValidFrags = IR_VariableAccess("numValidFrags", IR_IntegerDatatype)
  def fragmentOffset = IR_VariableAccess("fragmentOffset", IR_IntegerDatatype)
  def numFragsPerBlock = numValidFrags

  /*
    - without the reduction: 6 values for the position- and field data are written per cell
    - with the reduction: position and fields are output per grid node (one value per node) -> extend loop end by 1 to iterate through all nodes instead of cells
  */
  def nodalLoopEnd = if(Knowledge.swe_nodalReductionPrint) 1 else 0

  // offsets for vertices: (Lower0, Lower1, Lower2, Upper0, Upper1, Upper2)
  def vertexOffsets : Array[Int] = if(Knowledge.swe_nodalReductionPrint) {
    Array(0, 1, numCells_x+1, (numCells_x+1)+1, numCells_x+1, 1) // we're not writing 6 vertices per cell in this case
  } else {
    Array(0, 1, 2, 3, 4, 5)
  }

  // indices into node list to construct cells
  def offsetFragLoop = //(MPI_IV_MpiRank * Knowledge.domain_numFragmentsPerBlock + IR_LoopOverFragments.defIt) * numPointsPerFrag
    (fragmentOffset + IR_LoopOverFragments.defIt) * numPointsPerFrag

  def offsetLoopOverDim = if(Knowledge.swe_nodalReductionPrint) {
    IR_LoopOverDimensions.defItForDim(0) + IR_LoopOverDimensions.defItForDim(1) * (numCells_x+1)
  } else {
    6 * (IR_LoopOverDimensions.defItForDim(0) + IR_LoopOverDimensions.defItForDim(1) * numCells_x)
  }

  def connectivityForCell : Array[IR_Expression] = (0 until 6).map(v => offsetFragLoop + offsetLoopOverDim + vertexOffsets(v)).toArray
}
