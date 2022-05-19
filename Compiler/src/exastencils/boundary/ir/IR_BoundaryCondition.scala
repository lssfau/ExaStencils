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

package exastencils.boundary.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_Node
import exastencils.base.ir.IR_Statement
import exastencils.communication.NeighborInfo
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldAccess
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.grid.ir.IR_AtCellCenter
import exastencils.grid.ir.IR_AtFaceCenter
import exastencils.grid.ir.IR_AtNode
import exastencils.logger.Logger
import exastencils.waLBerla.ir.field.IR_WaLBerlaFieldAccess
import exastencils.waLBerla.ir.field.IR_WaLBerlaFieldCollection

/// IR_BoundaryCondition

trait IR_BoundaryCondition extends IR_Node {

  def generateFieldUpdate(field : IR_FieldLike, slot : IR_Expression, fragIdx : IR_Expression, neigh : NeighborInfo) : ListBuffer[IR_Statement] = {
    field.layout.localization match {
      case IR_AtNode                                           => generateFieldUpdatesNode(field, slot, fragIdx, neigh)
      case IR_AtFaceCenter(faceDim) if 0 != neigh.dir(faceDim) => generateFieldUpdatesNode(field, slot, fragIdx, neigh)
      case IR_AtCellCenter                                     => generateFieldUpdatesCell(field, slot, fragIdx, neigh)
      case IR_AtFaceCenter(_)                                  => generateFieldUpdatesCell(field, slot, fragIdx, neigh)
    }
  }

  def accessField(field : IR_FieldLike, slot : IR_Expression, fragIdx : IR_Expression, idx : IR_ExpressionIndex) = {
    // TODO refactor
    if (IR_WaLBerlaFieldCollection.exists(field.name, field.level))
      IR_WaLBerlaFieldAccess(IR_WaLBerlaFieldCollection.getByIdentifier(field.name, field.level).get, slot, idx)
    else field match {
      case f : IR_Field => IR_FieldAccess(f, slot, fragIdx, idx)
      case _            => Logger.error("Unknown field type.")
    }
  }

  def generateFieldUpdatesNode(field : IR_FieldLike, slot : IR_Expression, fragIdx : IR_Expression, neigh : NeighborInfo) : ListBuffer[IR_Statement]
  def generateFieldUpdatesCell(field : IR_FieldLike, slot : IR_Expression, fragIdx : IR_Expression, neigh : NeighborInfo) : ListBuffer[IR_Statement]
}

/// IR_NoBC

case object IR_NoBC extends IR_BoundaryCondition {
  exastencils.core.Duplicate.registerConstant(this)

  override def generateFieldUpdatesNode(field : IR_FieldLike, slot : IR_Expression, fragIdx : IR_Expression, neigh : NeighborInfo) = ListBuffer()
  override def generateFieldUpdatesCell(field : IR_FieldLike, slot : IR_Expression, fragIdx : IR_Expression, neigh : NeighborInfo) = ListBuffer()
}
