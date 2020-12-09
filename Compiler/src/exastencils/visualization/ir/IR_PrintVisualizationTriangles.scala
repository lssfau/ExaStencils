package exastencils.visualization.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_AddressOf
import exastencils.base.ir.IR_ArrayAccess
import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExternalFunctionReference
import exastencils.base.ir.IR_ForLoop
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_Lower
import exastencils.base.ir.IR_PreIncrement
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_UnknownDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_ArrayDatatype
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.parallelization.api.mpi.MPI_Reduce

// 2D only
// for a variable number of fragments per block

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

  def connectivityForCell : ListBuffer[IR_Expression] = ListBuffer() ++ (0 until 6).map(v => offsetFragLoop + offsetLoopOverDim + vertexOffsets(v))

  def communicateFragmentInfo(calculateFragOffset : Boolean = false) = {
    var statements = ListBuffer[IR_Statement]()

    // determine number of valid fragments per block and total number of valid fragments
    statements ++= ListBuffer(
      IR_VariableDeclaration(fragmentOffset, 0),
      IR_VariableDeclaration(numValidFrags, 0),
      IR_LoopOverFragments(IR_IfCondition(IR_IV_IsValidForDomain(0), IR_Assignment(numValidFrags, numValidFrags + 1))),
      IR_VariableDeclaration(numFrags, numValidFrags))

    if (Knowledge.mpi_enabled) {
      statements += MPI_Reduce(0, IR_AddressOf(numFrags), IR_IntegerDatatype, 1, "+")
      if(calculateFragOffset) {
        val actualFragsPerBlock = IR_VariableAccess("validFragsPerBlock", IR_ArrayDatatype(IR_IntegerDatatype, Knowledge.mpi_numThreads))
        val mpiInt = IR_VariableAccess(IR_IntegerDatatype.prettyprint_mpi, IR_UnknownDatatype)
        val mpiComm = IR_VariableAccess("mpiCommunicator", IR_UnknownDatatype)
        statements += IR_VariableDeclaration(actualFragsPerBlock)
        statements += IR_FunctionCall(
          IR_ExternalFunctionReference("MPI_Allgather"),
          IR_AddressOf(numValidFrags), 1, mpiInt,
          actualFragsPerBlock, 1, mpiInt, mpiComm
        )
        statements += IR_ForLoop(
          IR_VariableDeclaration(IR_IntegerDatatype, "curRank", 0),
          IR_Lower("curRank", MPI_IV_MpiRank),
          IR_PreIncrement("curRank"),
          IR_Assignment(fragmentOffset, IR_ArrayAccess(actualFragsPerBlock, "curRank"), "+=")
        )
      }
    }

    statements
  }
}
