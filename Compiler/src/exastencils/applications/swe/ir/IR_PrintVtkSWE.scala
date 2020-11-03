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
import exastencils.baseExt.ir._
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir._
import exastencils.parallelization.api.mpi._
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.IR_PrintVtkTriangles

/// IR_PrintVtkSWE

case class IR_PrintVtkSWE(var filename : IR_Expression, level : Int) extends IR_PrintVtkTriangles {
  def numDimsGrid = 2

  def numCells_x = etaDiscLower0.layout.layoutsPerDim(0).numInnerLayers
  def numCells_y = etaDiscLower0.layout.layoutsPerDim(1).numInnerLayers
  def numCells_z = 1
  def numPointsPerFrag = 6 * numCells_x * numCells_y

  def bath = IR_FieldCollection.getByIdentifier("bath", level).get

  def etaDiscLower0 = IR_FieldCollection.getByIdentifier("etaDiscLower0", level).get
  def etaDiscLower1 = IR_FieldCollection.getByIdentifier("etaDiscLower1", level).get
  def etaDiscLower2 = IR_FieldCollection.getByIdentifier("etaDiscLower2", level).get
  def etaDiscUpper0 = IR_FieldCollection.getByIdentifier("etaDiscUpper0", level).get
  def etaDiscUpper1 = IR_FieldCollection.getByIdentifier("etaDiscUpper1", level).get
  def etaDiscUpper2 = IR_FieldCollection.getByIdentifier("etaDiscUpper2", level).get

  def uDiscLower0 = IR_FieldCollection.getByIdentifier("uDiscLower0", level).get
  def uDiscLower1 = IR_FieldCollection.getByIdentifier("uDiscLower1", level).get
  def uDiscLower2 = IR_FieldCollection.getByIdentifier("uDiscLower2", level).get
  def uDiscUpper0 = IR_FieldCollection.getByIdentifier("uDiscUpper0", level).get
  def uDiscUpper1 = IR_FieldCollection.getByIdentifier("uDiscUpper1", level).get
  def uDiscUpper2 = IR_FieldCollection.getByIdentifier("uDiscUpper2", level).get

  def vDiscLower0 = IR_FieldCollection.getByIdentifier("vDiscLower0", level).get
  def vDiscLower1 = IR_FieldCollection.getByIdentifier("vDiscLower1", level).get
  def vDiscLower2 = IR_FieldCollection.getByIdentifier("vDiscLower2", level).get
  def vDiscUpper0 = IR_FieldCollection.getByIdentifier("vDiscUpper0", level).get
  def vDiscUpper1 = IR_FieldCollection.getByIdentifier("vDiscUpper1", level).get
  def vDiscUpper2 = IR_FieldCollection.getByIdentifier("vDiscUpper2", level).get

  def etaDisc = ListBuffer(etaDiscLower0, etaDiscLower1, etaDiscLower2, etaDiscUpper0, etaDiscUpper1, etaDiscUpper2)
  def uDisc = ListBuffer(uDiscLower0, uDiscLower1, uDiscLower2, uDiscUpper0, uDiscUpper1, uDiscUpper2)
  def vDisc = ListBuffer(vDiscLower0, vDiscLower1, vDiscLower2, vDiscUpper0, vDiscUpper1, vDiscUpper2)

  def optLocalOrderLower = IR_FieldCollection.getByIdentifier("local_orderLower0", level, suppressError = true)
  def optLocalOrderUpper = IR_FieldCollection.getByIdentifier("local_orderUpper0", level, suppressError = true)

  def someCellField = etaDiscLower0

  override def stmtsForNodeData : ListBuffer[IR_Statement] = {
    val stmts = ListBuffer[IR_Statement]()

    // add header
    val stream = newStream

    val numFields = 4 + (if (optLocalOrderLower.isDefined && optLocalOrderUpper.isDefined) 1 else 0)

    stmts ++= genStmtBlock(ListBuffer[IR_Statement](
      IR_IfCondition(MPI_IsRootProc(), ListBuffer[IR_Statement](
        IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
        IR_Print(stream, IR_StringConstant("POINT_DATA"), separator, numNodes, IR_Print.endl),
        IR_Print(stream, IR_StringConstant("FIELD"), separator, IR_StringConstant("FieldData"), separator, numFields, IR_Print.endl),
        IR_MemberFunctionCall(stream, "close")))))

    def addNodePrint(name : String, cellPrint : ListBuffer[IR_Expression]) = {
      val stream = newStream

      val print = IR_Print(stream, cellPrint)

      val initCells = ListBuffer[IR_Statement](
        IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess("std::ios::app", IR_UnknownDatatype)),
        IR_IfCondition(MPI_IsRootProc(),
          IR_Print(stream, IR_StringConstant(name), separator, 1, separator, numNodes, separator, IR_StringConstant("double"), IR_Print.endl)),
        IR_Print(stream, "std::scientific"),
        IR_LoopOverFragments(
          IR_IfCondition(IR_IV_IsValidForDomain(etaDiscLower0.domain.index),
            IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.layout.idxById("IB", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression)),
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => etaDiscLower0.layout.idxById("IE", dim) - Duplicate(etaDiscLower0.referenceOffset(dim)) : IR_Expression))),
              print)),
          IR_Print(stream, IR_Print.flush)),
        IR_MemberFunctionCall(stream, "close"))

      stmts ++= genStmtBlock(initCells)
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
    addNodePrint("eta", {
      var nodePrint = ListBuffer[IR_Expression]()
      etaDisc.foreach { eta =>
        nodePrint += IR_FieldAccess(eta, IR_IV_ActiveSlot(eta), IR_LoopOverDimensions.defIt(numDimsGrid))
        nodePrint += IR_Print.newline
      }
      nodePrint
    })

    // add u
    addNodePrint("u", {
      var nodePrint = ListBuffer[IR_Expression]()
      uDisc.foreach { u =>
        nodePrint += IR_FieldAccess(u, IR_IV_ActiveSlot(u), IR_LoopOverDimensions.defIt(numDimsGrid))
        nodePrint += IR_Print.newline
      }
      nodePrint
    })

    // add v
    addNodePrint("v", {
      var nodePrint = ListBuffer[IR_Expression]()
      vDisc.foreach { v =>
        nodePrint += IR_FieldAccess(v, IR_IV_ActiveSlot(v), IR_LoopOverDimensions.defIt(numDimsGrid))
        nodePrint += IR_Print.newline
      }
      nodePrint
    })

    // add local order
    if (optLocalOrderLower.isDefined && optLocalOrderUpper.isDefined) {
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

    stmts
  }

  override def stmtsForCellData : ListBuffer[IR_Statement] = ListBuffer()
}
