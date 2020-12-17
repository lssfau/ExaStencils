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
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.config._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir._
import exastencils.grid.ir.IR_AtCellCenter
import exastencils.grid.ir.IR_AtFaceCenter
import exastencils.grid.ir.IR_AtNode
import exastencils.grid.ir.IR_VF_CellCenterPerDim
import exastencils.grid.ir.IR_VF_NodePositionPerDim
import exastencils.logger.Logger
import exastencils.parallelization.api.mpi._
import exastencils.util.ir._

/// IR_PrintField

/*
object IR_PrintField {
  private var counter : Int = 0
  def getNewName() : String = {
    counter += 1
    "fieldPrintStream_%02d".format(counter)
  }
}
*/

case class IR_PrintField(
    var filename : IR_Expression,
    var field : IR_Field,
    var slot : IR_Expression,
    var ioInterface : IR_Expression,
    var includeGhostLayers : Boolean,
    var binaryOutput : Boolean = false,
    var separator : IR_Expression = IR_StringConstant(" "),
    var condition : IR_Expression = true,
    var dataset : IR_Expression = IR_NullExpression) extends IR_FieldIO(filename, field, slot, ioInterface, doWrite = true, onlyVals = false, includeGhostLayers, binaryOutput, separator, condition, dataset) {

  /*
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
  */

  val arrayIndexRange : Range = 0 until field.gridDatatype.resolveFlattendSize

  // writes comma-separated files in ascii mode, raw binaries otherwise (via locking)
  def printCSV() : ListBuffer[IR_Statement] = {
    // print coords for CSV files (Paraview)
    def getPos(dim : Int) : IR_Expression = {
      // TODO: add function to field (layout) to decide node/cell for given dim
      field.localization match {
        case IR_AtNode              => IR_VF_NodePositionPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(field.layout.numDimsGrid))
        case IR_AtCellCenter        => IR_VF_CellCenterPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(field.layout.numDimsGrid))
        case IR_AtFaceCenter(`dim`) => IR_VF_NodePositionPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(field.layout.numDimsGrid))
        case IR_AtFaceCenter(_)     => IR_VF_CellCenterPerDim.access(field.level, dim, IR_LoopOverDimensions.defIt(field.layout.numDimsGrid))
      }
    }
    val printPos = ListBuffer(IR_VariableAccess("std::defaultfloat", IR_UnknownDatatype)) ++
      (0 until field.layout.numDimsGrid).view.flatMap { dim => List(getPos(dim), separator) : List[IR_Expression] }

    // begin file access
    val fileAccess = generateFileAccess(Some(printPos))
    var statements : ListBuffer[IR_Statement] = ListBuffer()
    statements ++= fileAccess.createOrOpenFile()
    val fileHeader : ListBuffer[IR_Statement] = {
      var ret : ListBuffer[IR_Statement] = ListBuffer()
      var tmp : ListBuffer[IR_Statement] = ListBuffer()
      val openMode = if(Knowledge.mpi_enabled)
        IR_VariableAccess("std::ios::app", IR_UnknownDatatype) // file was already created by root process
      else
        IR_VariableAccess("std::ios::trunc", IR_UnknownDatatype)
      if (!binaryOutput && Knowledge.experimental_generateParaviewFiles) { // write header
        val streamName = IR_FieldIO.getNewStreamName()
        def streamType = IR_SpecialDatatype("std::ofstream")
        def stream = IR_VariableAccess(streamName, streamType)
        tmp += IR_ObjectInstantiation(streamType, streamName, filename, openMode)
        tmp += IR_Print(stream, "\"x,y,z," + arrayIndexRange.map(index => s"s$index").mkString(",") + "\"", IR_Print.endl)
        tmp += IR_MemberFunctionCall(stream, "close")
        if (Knowledge.mpi_enabled)
          ret += IR_IfCondition(MPI_IsRootProc(), tmp)
        else
          ret = tmp
      }
      ret
    }
    statements ++= fileHeader
    statements ++= fileAccess.accessFile(fieldAsDataBuffer)
    statements ++= fileAccess.closeFile()
    statements
  }

  def printXdmf() : ListBuffer[IR_Statement] = {
    // TODO
    ioInterface.asInstanceOf[IR_StringConstant].value.toLowerCase match {
      case "fpp" => ListBuffer(IR_NullStatement)
      case "mpiio" => ListBuffer(IR_NullStatement)
      case "hdf5" => ListBuffer(IR_NullStatement)
    }
  }

  def printNetCDF() : ListBuffer[IR_Statement] = {
    // TODO
    ListBuffer(IR_NullStatement)
  }

  override def expand() : Output[StatementList] = {

    /*
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
    printComponents ++= arrayIndexRange.view.flatMap { index =>
      val access = IR_FieldAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDimsData))
      if (numDimsData > numDimsGrid) // TODO: replace after implementing new field accessors
        access.index(numDimsData - 1) = index // TODO: assumes innermost dimension to represent vector index
      List(access, separator)
    }
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
      if (Knowledge.field_printFieldPrecision == -1)
        IR_Print(stream, "std::scientific")
      else
        IR_Print(stream, "std::scientific << std::setprecision(" + Knowledge.field_printFieldPrecision + ")"), //std::defaultfloat
      IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(field.domain.index),
          IR_LoopOverDimensions(numDimsData, IR_ExpressionIndexRange(
            IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.layout.idxById(fieldBegin, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression)),
            IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.layout.idxById(fieldEnd, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression))),
            IR_IfCondition(condition,
              IR_Print(stream, printComponents)))))
      ,
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
    */

    var statements : ListBuffer[IR_Statement] = ListBuffer()

    ioInterface.asInstanceOf[IR_StringConstant].value.toLowerCase match {
      case "lock"   => statements ++= printCSV()
      case "fpp" | "mpiio" | "hdf5"   => statements ++= printXdmf()
      case "nc"     => statements ++= printNetCDF()
      case "sion"   =>
        Logger.warn("Sion Files cannot directly be visualized. Defaulting to \"writeField\" implementation.")
        statements += generateFileAccess()
      case _        =>
        Logger.error("Ignoring call to \"printField\" with unsupported I/O interface: " + ioInterface)
    }

    statements
  }
}
