//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.visualization.ir.vtk

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_Expression
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.grid.ir.IR_VF_NodePositionPerDim
import exastencils.parallelization.api.mpi._
import exastencils.util.ir.IR_Print

/// IR_PrintVtkTriangles
// 2D only
// for a variable number of fragments per block

abstract class IR_PrintVtkTriangles extends IR_PrintVtk {
  def numCells = 2 * numCells_x * numCells_y * numCells_z * numFrags

  def nodeOffsets = ListBuffer(IR_ConstIndex(0, 0), IR_ConstIndex(1, 0), IR_ConstIndex(0, 1), IR_ConstIndex(1, 1), IR_ConstIndex(0, 1), IR_ConstIndex(1, 0))

  def numFrags = IR_VariableAccess("totalNumFrags", IR_IntegerDatatype) // Knowledge.domain_numFragmentsTotal
  def numValidFrags = IR_VariableAccess("numValidFrags", IR_IntegerDatatype)
  def fragmentOffset = IR_VariableAccess("fragmentOffset", IR_IntegerDatatype)

  def stmtsForPreparation() : ListBuffer[IR_Statement] = {
    var statements = ListBuffer[IR_Statement]()

    // determine number of valid fragments per block and total number of valid fragments
    statements ++= ListBuffer(
      IR_VariableDeclaration(fragmentOffset, 0),
      IR_VariableDeclaration(numValidFrags, 0),
      IR_LoopOverFragments(IR_IfCondition(IR_IV_IsValidForDomain(0), IR_Assignment(numValidFrags, numValidFrags + 1))),
      IR_VariableDeclaration(numFrags, numValidFrags))

    if (Knowledge.mpi_enabled)
      statements += MPI_Reduce(0, IR_AddressOf(numFrags), IR_IntegerDatatype, 1, "+")

    statements
  }

  override def stmtsForMeshVertices : ListBuffer[IR_Statement] = {
    val stream = newStream

    val triPrint = {
      nodeOffsets.map(offset => {
        var nodePrint = ListBuffer[IR_Expression]()
        for (d <- 0 until numDimsGrid) {
          nodePrint += IR_VF_NodePositionPerDim.access(level, d, IR_LoopOverDimensions.defIt(numDimsGrid) + offset)
          nodePrint += separator
        }
        for (_ <- numDimsGrid until 3) {
          nodePrint += 0
          nodePrint += separator
        }
        nodePrint = nodePrint.dropRight(1)
        nodePrint += IR_Print.newline
        IR_Print(stream, nodePrint) : IR_Statement
      })
    }

    val initPoints = ListBuffer[IR_Statement](
      IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
      IR_Print(stream, "std::scientific"), //std::defaultfloat
      IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(someCellField.domain.index),
          IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
            IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("IB", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression)),
            IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("IE", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression))),
            triPrint)),
        IR_Print.flush),
      IR_MemberFunctionCall(stream, "close"))

    genStmtBlock(initPoints)
  }

  override def stmtsForMeshCells : ListBuffer[IR_Statement] = {
    val stream = newStream

    val cellPrint = {
      val offset = //(MPI_IV_MpiRank * Knowledge.domain_numFragmentsPerBlock + IR_LoopOverFragments.defIt) * numPointsPerFrag
        (fragmentOffset + IR_LoopOverFragments.defIt) * numPointsPerFrag

      var cellPrint = ListBuffer[IR_Expression]()
      cellPrint += 3
      cellPrint += separator
      cellPrint += offset + 6 * (IR_LoopOverDimensions.defItForDim(0) + IR_LoopOverDimensions.defItForDim(1) * numCells_x) + 0
      cellPrint += separator
      cellPrint += offset + 6 * (IR_LoopOverDimensions.defItForDim(0) + IR_LoopOverDimensions.defItForDim(1) * numCells_x) + 1
      cellPrint += separator
      cellPrint += offset + 6 * (IR_LoopOverDimensions.defItForDim(0) + IR_LoopOverDimensions.defItForDim(1) * numCells_x) + 2
      cellPrint += IR_Print.newline

      cellPrint += 3
      cellPrint += separator
      cellPrint += offset + 6 * (IR_LoopOverDimensions.defItForDim(0) + IR_LoopOverDimensions.defItForDim(1) * numCells_x) + 3
      cellPrint += separator
      cellPrint += offset + 6 * (IR_LoopOverDimensions.defItForDim(0) + IR_LoopOverDimensions.defItForDim(1) * numCells_x) + 4
      cellPrint += separator
      cellPrint += offset + 6 * (IR_LoopOverDimensions.defItForDim(0) + IR_LoopOverDimensions.defItForDim(1) * numCells_x) + 5
      cellPrint += IR_Print.newline

      IR_Print(stream, cellPrint)
    }

    def sendRequest = IR_VariableAccess("sendRequest", "MPI_Request")

    def recvRequest = IR_VariableAccess("recvRequest", "MPI_Request")

    val initCells = ListBuffer[IR_Statement](
      IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
      IR_IfCondition(MPI_IsRootProc(), IR_Print(stream, IR_StringConstant("CELLS"), separator, numCells, separator, 4 * numCells, IR_Print.endl)),
      //IR_Print(stream, "std::scientific"), //std::defaultfloat
      IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(someCellField.domain.index),
          IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
            IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DLB", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression)),
            IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DRE", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression))),
            cellPrint)),
        IR_Print.flush),
      IR_MemberFunctionCall(stream, "close"),
      IR_Assignment(fragmentOffset, fragmentOffset + numValidFrags))

    if (Knowledge.mpi_enabled) {
      initCells.prepend(
        IR_IfCondition(MPI_IV_MpiRank > 0, ListBuffer[IR_Statement](
          IR_VariableDeclaration(recvRequest),
          MPI_Receive(IR_AddressOf(fragmentOffset), 1, IR_IntegerDatatype, MPI_IV_MpiRank - 1, 0, recvRequest),
          IR_FunctionCall(MPI_WaitForRequest.generateFctAccess(), IR_AddressOf(recvRequest)))))
      initCells.append(
        IR_IfCondition(MPI_IV_MpiRank < Knowledge.mpi_numThreads - 1, ListBuffer[IR_Statement](
          IR_VariableDeclaration(sendRequest),
          MPI_Send(IR_AddressOf(fragmentOffset), 1, IR_IntegerDatatype, MPI_IV_MpiRank + 1, 0, sendRequest),
          IR_FunctionCall(MPI_WaitForRequest.generateFctAccess(), IR_AddressOf(sendRequest)))))
    }

    genStmtBlock(initCells)
  }

  override def stmtsForCellTypes : ListBuffer[IR_Statement] = {
    val stream = newStream

    def it = IR_VariableAccess("i", IR_IntegerDatatype)

    val cellTypes = ListBuffer[IR_Statement](
      IR_IfCondition(MPI_IsRootProc(), ListBuffer[IR_Statement](
        IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
        IR_Print(stream, IR_StringConstant("CELL_TYPES"), separator, numCells, IR_Print.endl),
        IR_ForLoop(IR_VariableDeclaration(it, 0), IR_Lower(it, numCells), IR_PreIncrement(it),
          IR_Print(stream, ListBuffer[IR_Expression](5, separator))),
        IR_Print(stream, IR_Print.endl),
        IR_MemberFunctionCall(stream, "close"))))

    genStmtBlock(cellTypes)
  }
}
