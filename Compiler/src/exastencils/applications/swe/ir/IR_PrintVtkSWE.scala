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

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.field.ir._
import exastencils.grid.ir.IR_AtCellCenter
import exastencils.grid.ir.IR_AtNode
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi._
import exastencils.util.ir.IR_Print
import exastencils.visualization.ir.IR_PrintVtkTriangles

/// IR_PrintVtkSWE

case class IR_PrintVtkSWE(var filename : IR_Expression, level : Int, fieldAccesses : ListBuffer[IR_Expression]) extends IR_PrintVtkTriangles {
  def numDimsGrid = 2

  def numCells_x = someCellField.layout.layoutsPerDim(0).numInnerLayers
  def numCells_y = someCellField.layout.layoutsPerDim(1).numInnerLayers
  def numCells_z = 1
  def numPointsPerFrag = 6 * numCells_x * numCells_y

  def getBasenameDiscField(discField : ListBuffer[IR_Field]) = discField.map(_.name).reduce((a, b) => (a zip b).takeWhile(Function.tupled(_ == _)).map(_._1).mkString)

  /* validate arguments passed for "fieldAccesses" */
  // check if only field accesses were passed
  if (fieldAccesses.exists { acc => !acc.isInstanceOf[IR_FieldAccess] })
    Logger.error("\"IR_PrintVtkSWE\": Only field accesses are allowed for parameter: \"fields : IR_FieldAccess*\"")
  // check for correct localizations of fields
  val supportedLocalizations = List(IR_AtNode, IR_AtCellCenter)
  if (fieldAccesses.exists { case acc : IR_FieldAccess => !supportedLocalizations.contains(acc.field.localization) })
    Logger.error("\"IR_PrintVtkSWE\": Only node- or cell-centered fields are allowed.")
  // check if level specification is equal for each field
  if (fieldAccesses.exists { case field : IR_FieldAccess => field.level != level })
    Logger.error("\"IR_PrintVtkSWE\": Field accesses must occur on the same level.")

  /* extract different field types from the collection of fields passed to function */
  // get disc fields
  def discFields : ListBuffer[ListBuffer[IR_Field]] = {
    val discFieldComponents : mutable.HashSet[String] = mutable.HashSet() // no duplicate disc fields
    def throwErrorMsg = Logger.error(
      s""""IR_PrintVtkSWE": Not enough components specified for disc field (should always consist of 6 cell-centered components).
         | The convention is: discFieldLower0, discFieldLower1, discFieldLower2, discFieldUpper0, discFieldUpper1, discFieldUpper2.""".stripMargin)

    // begin with cell-centered field and build disc field from the following 6 components
    fieldAccesses.zipWithIndex.collect {
      case (acc : IR_FieldAccess, index) if acc.field.localization == IR_AtCellCenter && !discFieldComponents.contains(acc.name) =>
        // check if enough components were passed for a disc field
        if (index + 6 > fieldAccesses.length)
          throwErrorMsg
        // collect all 6 cell-centered components
        val components = fieldAccesses.slice(index, index + 6) collect {
          case accComponent : IR_FieldAccess if accComponent.field.localization == IR_AtCellCenter =>
            discFieldComponents.add(accComponent.name)
            accComponent.field
        }
        // check if disc field has 6 components and if they share a common prefix in their names
        if (components.length != 6)
          throwErrorMsg
        if (getBasenameDiscField(components).isEmpty)
          Logger.error("\"IR_PrintVtkSWE:\" Could not extract a common name from disc field components. Components do not belong to the same disc field.")

        components
    }.distinct
  }
  // get node fields
  def nodalFields : ListBuffer[IR_Field] = fieldAccesses.collect { case acc : IR_FieldAccess if acc.field.localization == IR_AtNode => acc.field }.distinct

  // etaDiscLower0 required to setup iteration spaces
  def etaDiscLower0 = IR_FieldCollection.getByIdentifier("etaDiscLower0", level).get
  def someCellField = etaDiscLower0

  override def stmtsForNodeData : ListBuffer[IR_Statement] = {
    val stmts = ListBuffer[IR_Statement]()

    // add header
    val stream = newStream

    val numFields = nodalFields.length + discFields.length

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
          IR_IfCondition(IR_IV_IsValidForDomain(someCellField.domain.index),
            IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("IB", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression)),
              IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => someCellField.layout.idxById("IE", dim) - Duplicate(someCellField.referenceOffset(dim)) : IR_Expression))),
              print)),
          IR_Print(stream, IR_Print.flush)),
        IR_MemberFunctionCall(stream, "close"))

      stmts ++= genStmtBlock(initCells)
    }

    // add nodal fields
    nodalFields.foreach { field =>
      addNodePrint(field.name, {
        var nodePrint = ListBuffer[IR_Expression]()
        nodeOffsets.foreach { offset =>
          nodePrint += IR_FieldAccess(field, IR_IV_ActiveSlot(field), IR_LoopOverDimensions.defIt(numDimsGrid) + offset)
          nodePrint += IR_Print.newline
        }
        nodePrint
      })
    }

    // add disc fields
    discFields.foreach { discField =>
      addNodePrint(getBasenameDiscField(discField), {
        var nodePrint = ListBuffer[IR_Expression]()
        discField.foreach { component =>
          nodePrint += IR_FieldAccess(component, IR_IV_ActiveSlot(component), IR_LoopOverDimensions.defIt(numDimsGrid))
          nodePrint += IR_Print.newline
        }
        nodePrint
      })
    }

    stmts
  }

  override def stmtsForCellData : ListBuffer[IR_Statement] = ListBuffer()
}
