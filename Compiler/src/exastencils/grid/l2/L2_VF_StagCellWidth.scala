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

package exastencils.grid.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2.L2_ImplicitConversion._
import exastencils.base.l2._
import exastencils.config.Knowledge
import exastencils.domain.l2.L2_Domain
import exastencils.grid.l3._
import exastencils.logger.Logger

/// L2_VF_StagCellWidthAsVec

object L2_VF_StagCellWidthAsVec {
  def find(level : Int, stagDim : Int) = L2_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellWidth", level).asInstanceOf[L2_VF_StagCellWidthAsVec]
  def access(level : Int, stagDim : Int, index : L2_ExpressionIndex) = L2_VirtualFieldAccess(find(level, stagDim), index)
}

case class L2_VF_StagCellWidthAsVec(
    var level : Int,
    var domain : L2_Domain,
    var stagDim : Int
) extends L2_VirtualFieldWithVec {

  override def name = s"vf_stag_${ stagDim }_cellWidth"
  override def knownAliases = ListBuffer(s"vf_${ L2_Localization.dimToString(stagDim) }StagCVWidth", s"vf_${ L2_Localization.dimToString(stagDim) }StagCellWidth")
  override def localization = L2_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def createDuplicate() = L2_VF_StagCellWidthAsVec(level, domain, stagDim)

  override def listPerDim = (0 until numDims).map(L2_VF_StagCellWidthPerDim.find(level, stagDim, _) : L2_VirtualField).to[ListBuffer]

  override def progressImpl() = L3_VF_StagCellWidthAsVec(level, domain.getProgressedObj(), stagDim)
}

/// L2_VF_StagCellWidthPerDim

object L2_VF_StagCellWidthPerDim {
  def find(level : Int, stagDim : Int, dim : Int) = L2_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellWidth_$dim", level).asInstanceOf[L2_VF_StagCellWidthPerDim]
  def access(level : Int, stagDim : Int, dim : Int, index : L2_ExpressionIndex) = L2_VirtualFieldAccess(find(level, stagDim, dim), index)
}

case class L2_VF_StagCellWidthPerDim(
    var level : Int,
    var domain : L2_Domain,
    var stagDim : Int,
    var dim : Int
) extends L2_VirtualFieldPerDim {

  override def name = s"vf_stag_${ stagDim }_cellWidth_$dim"
  override def knownAliases = {
    var aliases = ListBuffer(
      s"vf_${ L2_Localization.dimToString(stagDim) }StagCVWidth_${ L2_Localization.dimToString(dim) }",
      s"vf_${ L2_Localization.dimToString(stagDim) }StagCellWidth_$dim}",
      s"vf_stag_${ stagDim }_cellWidth_${ L2_Localization.dimToString(dim) }")
    if (dim == stagDim) aliases += s"vf_stagCVWidth_${ L2_Localization.dimToString(dim) }" // backwards compatibility
    aliases
  }
  override def localization = L2_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def createDuplicate() = L2_VF_StagCellWidthPerDim(level, domain, stagDim, dim)

  override def resolve(index : L2_ExpressionIndex) = {
    if (!Knowledge.grid_isStaggered) Logger.error("Trying to resolve a staggered quantity on a non-staggered grid; unsupported")

    if (Knowledge.grid_isAxisAligned) { // includes uniform grids
      if (dim == stagDim) // half of this cell and half of the left neighbor cell
        0.5 * (L2_VF_CellWidthPerDim.access(level, dim, L2_GridUtil.offsetIndex(index, -1, dim))
          + L2_VF_CellWidthPerDim.access(level, dim, index))
      else // just the un-staggered cell width
        L2_VF_CellWidthPerDim.access(level, dim, index)

    } else {
      Logger.error("Currently unsupported")
    }
  }

  override def progressImpl() = L3_VF_StagCellWidthPerDim(level, domain.getProgressedObj(), stagDim, dim)
}
