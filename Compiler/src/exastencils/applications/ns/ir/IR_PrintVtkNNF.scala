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
import exastencils.visualization.ir.vtk.IR_PrintVtkQuads

/// IR_PrintVtkNNF

case class IR_PrintVtkNNF(var filename : IR_Expression, level : Int, var resolveId : Int) extends IR_PrintVtkQuads with IR_PrintVisualizationNS with IR_PrintFieldAsciiNS {
  override def stmtsForNodeData : ListBuffer[IR_Statement] = ListBuffer()

  def fieldnames : ListBuffer[String] = ListBuffer("vel", "p", "rho", "mue", "gamma", "phi")

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
