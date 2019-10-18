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

package exastencils.grid.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_ImplicitConversion._
import exastencils.base.l4._
import exastencils.config.Knowledge
import exastencils.domain.l4.L4_Domain
import exastencils.grid.ir._
import exastencils.logger.Logger

/// L4_VF_CellWidthAsVec

object L4_VF_CellWidthAsVec {
  def find(level : Int) = L4_VirtualField.findVirtualField(s"vf_cellWidth", level).asInstanceOf[L4_VF_CellWidthAsVec]
  def access(level : Int, index : L4_ExpressionIndex) = L4_VirtualFieldAccess(find(level), index)
}

case class L4_VF_CellWidthAsVec(
    var level : Int,
    var domain : L4_Domain
) extends L4_VirtualFieldWithVec {

  override def name = "vf_cellWidth"
  override def knownAliases = ListBuffer("vf_cellWidthAsVec", "vf_gridWidthAsVec", "vf_gridWidth")
  override def localization = L4_AtCellCenter
  override def resolutionPossible = true

  override def createDuplicate() = L4_VF_CellWidthAsVec(level, domain)

  override def listPerDim = (0 until numDims).map(L4_VF_CellWidthPerDim.find(level, _) : L4_VirtualField).to[ListBuffer]

  override def progressImpl() = IR_VF_CellWidthAsVec(level, domain.getProgressedObj())
}

/// L4_VF_CellWidthPerDim

object L4_VF_CellWidthPerDim {
  def find(level : Int, dim : Int) = L4_VirtualField.findVirtualField(s"vf_cellWidth_$dim", level).asInstanceOf[L4_VF_CellWidthPerDim]
  def access(level : Int, dim : Int, index : L4_ExpressionIndex) = L4_VirtualFieldAccess(find(level, dim), index)
}

case class L4_VF_CellWidthPerDim(
    var level : Int,
    var domain : L4_Domain,
    var dim : Int
) extends L4_VirtualFieldPerDim {

  override def name = s"vf_cellWidth_$dim"
  override def knownAliases = ListBuffer(s"vf_cellWidth_${ L4_Localization.dimToString(dim) }", s"vf_gridWidth_$dim", s"vf_gridWidth_${ L4_Localization.dimToString(dim) }")
  override def localization = L4_AtCellCenter
  override def resolutionPossible = true

  override def createDuplicate() = L4_VF_CellWidthPerDim(level, domain, dim)

  override def resolve(index : L4_ExpressionIndex) = {
    if (Knowledge.grid_isUniform) {
      val levelIndex = level - Knowledge.minLevel
      dim match {
        case 0 => Knowledge.discr_hx(levelIndex)
        case 1 => Knowledge.discr_hy(levelIndex)
        case 2 => Knowledge.discr_hz(levelIndex)
      }

    } else if (Knowledge.grid_isAxisAligned) {
      L4_VF_NodePositionPerDim.access(level, dim, L4_GridUtil.offsetIndex(index, 1, dim)) - L4_VF_NodePositionPerDim.access(level, dim, index)

    } else {
      Logger.error("Currently unsupported")
    }
  }

  override def progressImpl() = IR_VF_CellWidthPerDim(level, domain.getProgressedObj(), dim)
}
