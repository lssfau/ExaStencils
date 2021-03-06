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

package exastencils.grid.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_Domain
import exastencils.logger.Logger

/// IR_VF_CellVolume

object IR_VF_CellVolume {
  def find(level : Int) = IR_VirtualField.findVirtualField(s"vf_cellVolume", level).asInstanceOf[IR_VF_CellVolume]
  def access(level : Int, index : IR_ExpressionIndex) = IR_VirtualFieldAccess(find(level), index)
}

case class IR_VF_CellVolume(
    var level : Int,
    var domain : IR_Domain
) extends IR_VirtualFieldWithScalar {

  override def name = "vf_cellVolume"
  override def knownAliases = ListBuffer("vf_cellVol")
  override def localization = IR_AtCellCenter
  override def resolutionPossible = true

  override def createDuplicate() = IR_VF_CellVolume(level, domain)

  override def resolve(index : IR_ExpressionIndex) = {
    if (Knowledge.grid_isAxisAligned) // includes uniform grids
      (0 until domain.numDims).map(dim => IR_VF_CellWidthPerDim.access(level, dim, Duplicate(index)) : IR_Expression).reduce(_ * _)
    else
      Logger.error("Currently unsupported")
  }
}
