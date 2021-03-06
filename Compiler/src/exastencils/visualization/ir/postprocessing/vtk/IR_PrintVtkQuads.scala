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

package exastencils.visualization.ir.postprocessing.vtk

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.grid.ir.IR_VF_NodePositionPerDim
import exastencils.parallelization.api.mpi._
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.postprocessing.IR_PrintVisualizationQuads

/// IR_PrintVtkQuads

abstract class IR_PrintVtkQuads extends IR_PrintVtk with IR_PrintVisualizationQuads {

  override def stmtsForMeshVertices : ListBuffer[IR_Statement] = {
    val stream = newStream

    val pointPrint = {
      var nodePrint = ListBuffer[IR_Expression]()
      for (d <- 0 until numDimsGrid) {
        nodePrint += IR_VF_NodePositionPerDim.access(level, d, IR_LoopOverDimensions.defIt(numDimsGrid))
        nodePrint += separator
      }
      for (_ <- numDimsGrid until 3) {
        nodePrint += 0
        nodePrint += separator
      }
      nodePrint = nodePrint.dropRight(1)
      nodePrint += IR_Print.newline
      IR_Print(stream, nodePrint)
    }

    val initPoints = ListBuffer[IR_Statement](
      IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
      IR_Print(stream, "std::scientific"), //std::defaultfloat
      IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
          IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
            IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DLB", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression)),
            IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DRE", dim) + 1 - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression))),
            pointPrint)),
        IR_Print(stream, IR_Print.flush)),
      IR_MemberFunctionCall(stream, "close"))

    genStmtBlock(initPoints)
  }

  override def stmtsForMeshCells : ListBuffer[IR_Statement] = {
    val stream = newStream

    val cellPrint = {

      var cellPrint = ListBuffer[IR_Expression]()
      connectivityForCell().foreach(conn => {
        cellPrint += separator
        cellPrint += conn
      })

      numDimsGrid match {
        case 2 =>
          cellPrint.prepend(4)
          cellPrint.append(IR_Print.newline)
        case 3 =>
          cellPrint.prepend(8)
          cellPrint.append(IR_Print.newline)
      }

      IR_Print(stream, cellPrint)
    }

    val initCells = ListBuffer[IR_Statement](
      IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
      numDimsGrid match {
        case 2 => IR_IfCondition(MPI_IsRootProc(), IR_Print(stream, IR_StringConstant("CELLS"), separator, numCells, separator, 5 * numCells, IR_Print.endl))
        case 3 => IR_IfCondition(MPI_IsRootProc(), IR_Print(stream, IR_StringConstant("CELLS"), separator, numCells, separator, 9 * numCells, IR_Print.endl))
      },
      //IR_Print(stream, "std::scientific"), //std::defaultfloat
      IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(domainIndex),
          IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
            IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DLB", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression)),
            IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("DRE", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression))),
            cellPrint)),
        IR_Print(stream, IR_Print.flush)),
      IR_MemberFunctionCall(stream, "close"))

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
          numDimsGrid match {
            case 2 => IR_Print(stream, ListBuffer[IR_Expression](9, separator))
            case 3 => IR_Print(stream, ListBuffer[IR_Expression](12, separator))
          }
        ),
        IR_Print(stream, IR_Print.endl),
        IR_MemberFunctionCall(stream, "close"))))

    genStmtBlock(cellTypes)
  }
}
