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

package exastencils.applications.swe.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.field.ir.IR_Field
import exastencils.parallelization.api.mpi._
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.postprocessing.vtk.IR_PrintVtkTriangles

/// IR_PrintVtkSWE

case class IR_PrintVtkSWE(
    var filename : IR_Expression,
    level : Int,
    var resolveId : Int,
    var nodalFieldCollection : ListBuffer[IR_Field],
    var discFieldCollection : ListBuffer[ListBuffer[IR_Field]]
) extends IR_PrintVtkTriangles with IR_PrintVisualizationSWE with IR_PrintFieldsAsciiSWE {

  override def printField(name : String, stream : IR_VariableAccess, loopBody : ListBuffer[IR_Statement]) : ListBuffer[IR_Statement] = ListBuffer[IR_Statement](
    IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
    IR_IfCondition(MPI_IsRootProc(),
      IR_Print(stream, IR_StringConstant(name), separator, 1, separator, numNodes, separator, IR_StringConstant("double"), IR_Print.endl))) ++
    super.printField(name, stream, loopBody) :+
    IR_ExpressionStatement(IR_MemberFunctionCall(stream, "close"))

  override def stmtsForNodeData : ListBuffer[IR_Statement] = {
    val stmts = ListBuffer[IR_Statement]()

    // add header
    val stream = newStream
    stmts ++= genStmtBlock(ListBuffer[IR_Statement](
      IR_IfCondition(MPI_IsRootProc(), ListBuffer[IR_Statement](
        IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
        IR_Print(stream, IR_StringConstant("POINT_DATA"), separator, numNodes, IR_Print.endl),
        IR_Print(stream, IR_StringConstant("FIELD"), separator, IR_StringConstant("FieldData"), separator, numFields, IR_Print.endl),
        IR_MemberFunctionCall(stream, "close")))))

    // implemented in IR_PrintFieldsAsciiSWE
    nodalFields.values.foreach { field =>
      stmts ++= genStmtBlock(printNodalField(field))
    }
    discFields.values.foreach { discField =>
      stmts ++= genStmtBlock(printDiscField(discField))
    }

    stmts
  }

  override def stmtsForCellData : ListBuffer[IR_Statement] = ListBuffer()
}
