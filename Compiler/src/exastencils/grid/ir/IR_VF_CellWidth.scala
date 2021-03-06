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

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.domain.ir.IR_Domain
import exastencils.logger.Logger

/// IR_VF_CellWidthAsVec

object IR_VF_CellWidthAsVec {
  def find(level : Int) = IR_VirtualField.findVirtualField(s"vf_cellWidth", level).asInstanceOf[IR_VF_CellWidthAsVec]
  def access(level : Int, index : IR_ExpressionIndex) = IR_VirtualFieldAccess(find(level), index)
}

case class IR_VF_CellWidthAsVec(
    var level : Int,
    var domain : IR_Domain
) extends IR_VirtualFieldWithVec {

  override def name = "vf_cellWidth"
  override def knownAliases = ListBuffer("vf_cellWidthAsVec", "vf_gridWidthAsVec", "vf_gridWidth")
  override def localization = IR_AtCellCenter
  override def resolutionPossible = true

  override def createDuplicate() = IR_VF_CellWidthAsVec(level, domain)

  override def listPerDim = (0 until numDims).map(IR_VF_CellWidthPerDim.find(level, _) : IR_VirtualField).to[ListBuffer]
}

/// IR_VF_CellWidthPerDim

object IR_VF_CellWidthPerDim {
  def find(level : Int, dim : Int) = IR_VirtualField.findVirtualField(s"vf_cellWidth_$dim", level).asInstanceOf[IR_VF_CellWidthPerDim]
  def access(level : Int, dim : Int, index : IR_ExpressionIndex) = IR_VirtualFieldAccess(find(level, dim), index)
}

case class IR_VF_CellWidthPerDim(
    var level : Int,
    var domain : IR_Domain,
    var dim : Int
) extends IR_VirtualFieldPerDim {

  override def name = s"vf_cellWidth_$dim"
  override def knownAliases = ListBuffer(s"vf_cellWidth_${ IR_Localization.dimToString(dim) }", s"vf_gridWidth_$dim", s"vf_gridWidth_${ IR_Localization.dimToString(dim) }")
  override def localization = IR_AtCellCenter
  override def resolutionPossible = true

  override def createDuplicate() = IR_VF_CellWidthPerDim(level, domain, dim)

  override def resolve(index : IR_ExpressionIndex) = {
    if (Knowledge.grid_isUniform) {
      val levelIndex = level - Knowledge.minLevel
      dim match {
        case 0 => Knowledge.discr_hx(levelIndex)
        case 1 => Knowledge.discr_hy(levelIndex)
        case 2 => Knowledge.discr_hz(levelIndex)
      }

    } else if (Knowledge.grid_isAxisAligned) {
      IR_VF_NodePositionPerDim.access(level, dim, IR_GridUtil.offsetIndex(index, 1, dim)) - IR_VF_NodePositionPerDim.access(level, dim, index)

    } else {
      Logger.error("Currently unsupported")
    }
  }
}
