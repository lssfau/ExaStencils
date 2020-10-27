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
import exastencils.domain.ir.IR_IV_IsValidForDomain
import exastencils.grid.ir._
import exastencils.parallelization.api.mpi.MPI_IV_MpiRank
import exastencils.util.ir._

/// IR_ReadField

object IR_ReadField {
  private var counter : Int = 0
  def getNewName() : String = {
    counter += 1
    "fieldReadStream_%02d".format(counter)
  }

  private var fileNameCounter : Int = 0
  def getNewFileName() : String = {
    fileNameCounter += 1
    "fieldName_%02d".format(fileNameCounter)
  }
}

case class IR_ReadField(
    var filename : IR_Expression,
    var field : IR_Field,
    var slot : IR_Expression,
    var condition : IR_Expression = true,
    var includeGhostLayers : Boolean = false) extends IR_Statement with IR_Expandable {

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

    // TODO: incorporate component accesses
    val arrayIndexRange = 0 until field.gridDatatype.resolveFlattendSize

    val streamName = IR_ReadField.getNewName()

    def streamType = IR_SpecialDatatype("std::ifstream")

    def stream = IR_VariableAccess(streamName, streamType)

    val read = IR_Read(stream)
    //    arrayIndexRange.foreach { index =>
    val access = IR_FieldAccess(field, Duplicate(slot), IR_LoopOverDimensions.defIt(numDimsData))
    //      if (numDimsData > numDimsGrid) // TODO: replace after implementing new field accessors
    //        access.index(numDimsData - 2) = index // TODO: other hodt
    read.toRead += access
    //    }

    var statements : ListBuffer[IR_Statement] = ListBuffer()

    filename match {
      case filenameStrConst : IR_StringConstant =>
        val str : String = filenameStrConst.value
        val strSplit = ListBuffer(str.split("\\$blockId") : _ *)

        val strListMpi = strSplit.flatMap(e => MPI_IV_MpiRank :: IR_StringConstant(e) :: Nil).tail

        val mpiFileName = IR_VariableAccess(IR_ReadField.getNewFileName(), IR_StringDatatype)
        statements += IR_VariableDeclaration(mpiFileName)
        statements += IR_BuildString(mpiFileName, strListMpi)
        statements += IR_ObjectInstantiation(stream, Duplicate(mpiFileName))
      case _                                    =>
        statements += IR_ObjectInstantiation(stream, Duplicate(filename))
    }

    def beginId = if (includeGhostLayers) "GLB" else "DLB"

    def endId = if (includeGhostLayers) "GRE" else "DRE"

    statements +=
      IR_LoopOverFragments(
        IR_IfCondition(IR_IV_IsValidForDomain(field.domain.index),
          IR_LoopOverDimensions(numDimsData, IR_ExpressionIndexRange(
            IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.layout.idxById(beginId, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression)),
            IR_ExpressionIndex((0 until numDimsData).toArray.map(dim => field.layout.idxById(endId, dim) - Duplicate(field.referenceOffset(dim)) : IR_Expression))),
            IR_IfCondition(condition, read))))

    statements += IR_MemberFunctionCall(stream, "close")

    statements
  }
}
