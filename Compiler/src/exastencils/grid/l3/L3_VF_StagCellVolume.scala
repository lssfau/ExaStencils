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

package exastencils.grid.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.l3.L3_Domain
import exastencils.grid.l4.L4_VF_StagCellVolume
import exastencils.logger.Logger

/// L3_VF_StagCellVolume

object L3_VF_StagCellVolume {
  def find(level : Int, stagDim : Int) = L3_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellVolume", level).asInstanceOf[L3_VF_StagCellVolume]
  def access(level : Int, stagDim : Int, index : L3_ExpressionIndex) = L3_VirtualFieldAccess(find(level, stagDim), index)
}

case class L3_VF_StagCellVolume(
    var level : Int,
    var domain : L3_Domain,
    var stagDim : Int
) extends L3_VirtualFieldWithScalar {

  override def name = s"vf_stag_${ stagDim }_cellVolume"
  override def knownAliases = ListBuffer(s"vf_stag_${ stagDim }_cellVol",
    s"vf_${ L3_Localization.dimToString(stagDim) }StagCellVolume", s"vf_${ L3_Localization.dimToString(stagDim) }StagCellVol")

  override def localization = L3_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def createDuplicate() = L3_VF_StagCellVolume(level, domain, stagDim)

  override def resolve(index : L3_ExpressionIndex) = {
    if (!Knowledge.grid_isStaggered) Logger.error("Trying to resolve a staggered quantity on a non-staggered grid; unsupported")

    if (Knowledge.grid_isAxisAligned) // includes uniform grids
      (0 until domain.numDims).map(dim => L3_VF_StagCellWidthPerDim.access(level, stagDim, dim, Duplicate(index)) : L3_Expression).reduce(_ * _)
    else
      Logger.error("Currently unsupported")
  }

  override def progressImpl() = L4_VF_StagCellVolume(level, domain.getProgressedObj(), stagDim)
}
