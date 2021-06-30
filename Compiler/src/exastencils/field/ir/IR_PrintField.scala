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

package exastencils.field.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir._
import exastencils.domain.ir._
import exastencils.grid.ir._
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi._
import exastencils.util.ir._

/// IR_PrintField

object IR_PrintField {
  private var counter : Int = 0
  def getNewName() : String = {
    counter += 1
    "fieldPrintStream_%02d".format(counter)
  }
}

case class IR_PrintField(
    var filename : IR_Expression,
    var field : IR_Field,
    var slot : IR_Expression,
    var condition : IR_Expression = true,
    var includeGhostLayers : Boolean = false,
    var onlyValues : Boolean = false,
    var binary : Boolean = false) extends IR_Statement with IR_Expandable {

  def numDimsGrid = field.layout.numDimsGrid
  def numDimsData = field.layout.numDimsData

  def getPos(field : IR_Field, dim : Int) : IR_Expression = {
    // TODO: add function to field (layout) to decide node/cell for given dim
    field.localization match {
      case IR_AtNode              => IR_VF_NodePositionPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
      case IR_AtCellCenter        => IR_VF_CellCenterPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
      case IR_AtFaceCenter(`dim`) => IR_VF_NodePositionPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
      case IR_AtFaceCenter(_)     => IR_VF_CellCenterPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(numDimsGrid))
    }
  }

  override def expand() : Output[StatementList] = {
    if (!Settings.additionalIncludes.contains("fstream"))
      Settings.additionalIncludes += "fstream"
    if (!Settings.additionalIncludes.contains("iomanip"))
      Settings.additionalIncludes += "iomanip"

    // TODO: incorporate component accesses
    val arrayIndexRange = 0 until field.gridDatatype.resolveFlattendSize

    def separator = IR_StringConstant(if (binary) "" else if (Knowledge.experimental_generateParaviewFiles) "," else " ")

    val streamName = IR_PrintField.getNewName()

    def streamType = IR_SpecialDatatype("std::ofstream")

    def stream = IR_VariableAccess(streamName, streamType)

    val fileHeader = {
      var ret : IR_Statement = IR_NullStatement
      if (Knowledge.experimental_generateParaviewFiles) {
        ret = IR_Print(stream, "\"x,y,z," + arrayIndexRange.map(index => s"s$index").mkString(",") + "\"", IR_Print.endl)
        if (Knowledge.mpi_enabled)
          ret = IR_IfCondition(MPI_IsRootProc(), ret)
      }
      ret
    }

    val printComponents = ListBuffer[IR_Expression]()
    if (!onlyValues) {
      printComponents += "std::defaultfloat"
      printComponents ++= (0 until numDimsGrid).view.flatMap { dim => List(getPos(field, dim), separator) }
    }
    printComponents += "std::scientific"
    val accesses = if (numDimsData > numDimsGrid) {
      field.gridDatatype match {
        case mat : IR_MatrixDatatype =>
          Array.range(0, mat.sizeM).flatMap(rows =>
            Array.range(0, mat.sizeN).map(cols =>
              IR_ExpressionIndex(IR_LoopOverDimensions.defIt(numDimsGrid).indices :+ IR_IntegerConstant(rows) :+ IR_IntegerConstant(cols))))
        case _ : IR_ScalarDatatype   =>
          Array(IR_LoopOverDimensions.defIt(numDimsGrid))
        case _                       =>
          Logger.error("Unsupported higher dimensional datatype used for I/O interface.")
      }
    } else {
      Array(IR_LoopOverDimensions.defIt(numDimsGrid))
    }
    printComponents ++= accesses.flatMap(acc => ListBuffer(IR_FieldAccess(field, Duplicate(slot), acc), separator))
    printComponents += IR_Print.endl

    val fieldBegin = if (includeGhostLayers) "GLB" else "DLB"
    val fieldEnd = if (includeGhostLayers) "GRE" else "DRE"

    var openMode = if (Knowledge.mpi_enabled) "std::ios::app" else "std::ios::trunc"
    if (binary)
      openMode += " | std::ios::binary"

    // TODO: less monolithic code
    var innerLoop = ListBuffer[IR_Statement](
      IR_ObjectInstantiation(stream, Duplicate(filename), IR_VariableAccess(openMode, IR_UnknownDatatype)),
      fileHeader,
      if (Knowledge.field_printFieldPrecision == -1) {
        IR_Print(stream, "std::scientific")
      } else {
        IR_Print(stream, "std::scientific << std::setprecision(" + Knowledge.field_printFieldPrecision + ")")
      },
      IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(field.domain.index),
          IR_LoopOverDimensions(numDimsGrid, IR_ExpressionIndexRange(
            IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => field.layout.idxById(fieldBegin, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression)),
            IR_ExpressionIndex((0 until numDimsGrid).toArray.map(dim => field.layout.idxById(fieldEnd, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression))),
            IR_IfCondition(condition,
              IR_Print(stream, printComponents))))),
      IR_MemberFunctionCall(stream, "close")
    )

    var statements : ListBuffer[IR_Statement] = ListBuffer()

    if (Knowledge.mpi_enabled) {
      statements += IR_IfCondition(MPI_IsRootProc(),
        ListBuffer[IR_Statement](
          IR_ObjectInstantiation(streamType, streamName, Duplicate(filename), IR_VariableAccess("std::ios::trunc", IR_UnknownDatatype)),
          IR_MemberFunctionCall(stream, "close")))

      statements += MPI_Sequential(innerLoop)
    } else {
      statements ++= innerLoop
    }

    statements
  }
}
