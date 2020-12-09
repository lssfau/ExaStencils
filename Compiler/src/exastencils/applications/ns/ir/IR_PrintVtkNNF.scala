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

package exastencils.applications.ns.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.parallelization.api.mpi._
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.IR_PrintVtkQuads

/// IR_PrintVtkNNF

case class IR_PrintVtkNNF(var filename : IR_Expression, level : Int) extends IR_PrintVtkQuads with IR_PrintVisualizationNS with IR_PrintFieldAsciiNS {
  override def stmtsForNodeData : ListBuffer[IR_Statement] = ListBuffer()

  def numFields = 6

  override def printField(name : String, stream : IR_VariableAccess, loopBody : ListBuffer[IR_Statement], numComponents : Int = 1) : ListBuffer[IR_Statement] = ListBuffer(
    IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
    IR_IfCondition(MPI_IsRootProc(),
      IR_Print(stream, IR_StringConstant(name), separator, numComponents, separator, numCells, separator, IR_StringConstant("double"), IR_Print.endl))) ++
    super.printField(name, stream, loopBody) :+
    IR_ExpressionStatement(IR_MemberFunctionCall(stream, "close"))

  override def stmtsForCellData : ListBuffer[IR_Statement] = {
    val stmts = ListBuffer[IR_Statement]()

    val stream = newStream

    stmts ++= genStmtBlock(ListBuffer[IR_Statement](
      IR_IfCondition(MPI_IsRootProc(), ListBuffer[IR_Statement](
        IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
        IR_Print(stream, IR_StringConstant("CELL_DATA"), separator, numCells, IR_Print.endl),
        IR_Print(stream, IR_StringConstant("FIELD"), separator, IR_StringConstant("FieldData"), separator, numFields, IR_Print.endl),
        IR_MemberFunctionCall(stream, "close")))))

    /*
    def addCellPrint(name : String, cellPrint : ListBuffer[IR_Expression], numComponents : Int = 1) = {
      val stream = newStream

      val print = IR_Print(stream, cellPrint)

      val initCells = ListBuffer[IR_Statement](
        IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
        IR_IfCondition(MPI_IsRootProc(),
          IR_Print(stream, IR_StringConstant(name), separator, numComponents, separator, numCells, separator, IR_StringConstant("double"), IR_Print.endl)),
        IR_Print(stream, "std::scientific"),
        IR_LoopOverFragments(
          IR_IfCondition(IR_IV_IsValidForDomain(p.domain.index),
            IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.layout.idxById("DLB", dim) - Duplicate(p.referenceOffset(dim)) : IR_Expression)),
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => p.layout.idxById("DRE", dim) - Duplicate(p.referenceOffset(dim)) : IR_Expression))),
              print)),
          IR_Print(stream, IR_Print.flush)),
        IR_MemberFunctionCall(stream, "close"))

      stmts ++= genStmtBlock(initCells)
    }

    // add vel
    addCellPrint("vel", {
      var cellPrint = ListBuffer[IR_Expression]()
      cellPrint += meanU
      cellPrint += separator
      cellPrint += meanV
      if (numDimsGrid > 2) {
        cellPrint += separator
        cellPrint += meanW
      }
      cellPrint += IR_Print.newline
    }, numDimsGrid)

    // add p
    addCellPrint("p", {
      var cellPrint = ListBuffer[IR_Expression]()
      cellPrint += IR_FieldAccess(p, IR_IV_ActiveSlot(p), IR_LoopOverDimensions.defIt(numDimsGrid))
      cellPrint += IR_Print.newline
    })

    // add rho
    addCellPrint("rho", {
      var cellPrint = ListBuffer[IR_Expression]()
      cellPrint += IR_FieldAccess(rho, IR_IV_ActiveSlot(rho), IR_LoopOverDimensions.defIt(numDimsGrid))
      cellPrint += IR_Print.newline
    })

    // add rho
    addCellPrint("mue", {
      var cellPrint = ListBuffer[IR_Expression]()
      cellPrint += IR_FieldAccess(mue, IR_IV_ActiveSlot(mue), IR_LoopOverDimensions.defIt(numDimsGrid))
      cellPrint += IR_Print.newline
    })

    // add rho
    addCellPrint("gamma", {
      var cellPrint = ListBuffer[IR_Expression]()
      cellPrint += IR_FieldAccess(gamma, IR_IV_ActiveSlot(gamma), IR_LoopOverDimensions.defIt(numDimsGrid))
      cellPrint += IR_Print.newline
    })

    // add phi
    addCellPrint("phi", {
      var cellPrint = ListBuffer[IR_Expression]()
      cellPrint += IR_FieldAccess(phi, IR_IV_ActiveSlot(phi), IR_LoopOverDimensions.defIt(numDimsGrid))
      cellPrint += IR_Print.newline
    })
    */

    // implemented in IR_PrintFieldsAsciiNS
    stmts ++= genStmtBlock(printVel())
    stmts ++= genStmtBlock(printP())
    stmts ++= genStmtBlock(printRho())
    stmts ++= genStmtBlock(printMue())
    stmts ++= genStmtBlock(printGamma())
    stmts ++= genStmtBlock(printPhi())

    stmts
  }
}
