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
import exastencils.parallelization.api.mpi._
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.IR_PrintVtkTriangles

/// IR_PrintVtkSWE

case class IR_PrintVtkSWE(var filename : IR_Expression, level : Int) extends IR_PrintVtkTriangles with IR_PrintVisualizationSWE with IR_PrintFieldsAsciiSWE {

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

    /*
    def addNodePrint(name : String, cellPrint : ListBuffer[IR_Expression]) = {
      val stream = newStream

      val print = IR_Print(stream, cellPrint)

      val loopBody = IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
        IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.layout.idxById("IB", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression)),
        IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => nodalLoopEnd + etaDiscLower0.layout.idxById("IE", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression))),
        print)

      stmts ++= genStmtBlock(initCells(name, stream, ListBuffer(loopBody)))
    }

    def addReducedNodePrint(name : String, discFields : ListBuffer[IR_Field]) {
      val stream = newStream

      stmts ++= genStmtBlock(initCells(name, stream, reducedCellPrint(stream, discFields)))
    }

    // add bath
    addNodePrint("bath", {
      var nodePrint = ListBuffer[IR_Expression]()
      nodeOffsets.foreach { offset =>
        nodePrint += IR_FieldAccess(bath, IR_IV_ActiveSlot(bath), IR_LoopOverDimensions.defIt(numDimsGrid) + offset)
        nodePrint += IR_Print.newline
      }
      nodePrint
    })

    // add eta
    if(Knowledge.swe_nodalReductionPrint) {
      addReducedNodePrint("eta", etaDisc)
    } else {
      addNodePrint("eta", {
        var nodePrint = ListBuffer[IR_Expression]()
        etaDisc.foreach { eta =>
          nodePrint += IR_FieldAccess(eta, IR_IV_ActiveSlot(eta), IR_LoopOverDimensions.defIt(numDimsGrid))
          nodePrint += IR_Print.newline
        }
        nodePrint
      })
    }

    // add u
    if(Knowledge.swe_nodalReductionPrint) {
      addReducedNodePrint("u", uDisc)
    } else {
      addNodePrint("u", {
        var nodePrint = ListBuffer[IR_Expression]()
        uDisc.foreach { u =>
          nodePrint += IR_FieldAccess(u, IR_IV_ActiveSlot(u), IR_LoopOverDimensions.defIt(numDimsGrid))
          nodePrint += IR_Print.newline
        }
        nodePrint
      })
    }

    // add v
    if(Knowledge.swe_nodalReductionPrint) {
      addReducedNodePrint("v", vDisc)
    } else {
      addNodePrint("v", {
        var nodePrint = ListBuffer[IR_Expression]()
        vDisc.foreach { v =>
          nodePrint += IR_FieldAccess(v, IR_IV_ActiveSlot(v), IR_LoopOverDimensions.defIt(numDimsGrid))
          nodePrint += IR_Print.newline
        }
        nodePrint
      })
    }

    // add local order
    if (optLocalOrderLower.isDefined && optLocalOrderUpper.isDefined) {
      if(Knowledge.swe_nodalReductionPrint) {
        addReducedNodePrint("order", ListBuffer() ++ (0 until 3).map(_ => optLocalOrderLower.get) ++ (0 until 3).map(_ => optLocalOrderUpper.get))
      } else {
        addNodePrint("order", {
          var nodePrint = ListBuffer[IR_Expression]()
          List(optLocalOrderLower.get, optLocalOrderUpper.get).foreach { f =>
            for (_ <- 0 until 3) { // TODO: cell data instead of
              nodePrint += IR_FieldAccess(f, IR_IV_ActiveSlot(f), IR_LoopOverDimensions.defIt(numDimsGrid))
              nodePrint += IR_Print.newline
            }
          }
          nodePrint
        })
      }
    }
    */

    // implemented in IR_PrintFieldsAsciiSWE
    stmts ++= genStmtBlock(printBath())
    stmts ++= genStmtBlock(printEta())
    stmts ++= genStmtBlock(printU())
    stmts ++= genStmtBlock(printV())
    stmts ++= genStmtBlock(printOrder())

    stmts
  }

  override def stmtsForCellData : ListBuffer[IR_Statement] = ListBuffer()
}
