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

import exastencils.base.l4._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.l4.L4_Domain
import exastencils.grid.ir.IR_VF_StagCellVolume
import exastencils.logger.Logger

/// L4_VF_StagCellVolume

object L4_VF_StagCellVolume {
  def find(level : Int, stagDim : Int) = L4_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellVolume", level).asInstanceOf[L4_VF_StagCellVolume]
  def access(level : Int, stagDim : Int, index : L4_ExpressionIndex) = L4_VirtualFieldAccess(find(level, stagDim), index)
}

case class L4_VF_StagCellVolume(
    var level : Int,
    var domain : L4_Domain,
    var stagDim : Int
) extends L4_VirtualFieldWithScalar {

  override def name = s"vf_stag_${ stagDim }_cellVolume"
  override def knownAliases = ListBuffer(s"vf_stag_${ stagDim }_cellVol",
    s"vf_${ L4_Localization.dimToString(stagDim) }StagCellVolume", s"vf_${ L4_Localization.dimToString(stagDim) }StagCellVol")

  override def localization = L4_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def createDuplicate() = L4_VF_StagCellVolume(level, domain, stagDim)

  override def resolve(index : L4_ExpressionIndex) = {
    if (!Knowledge.grid_isStaggered) Logger.error("Trying to resolve a staggered quantity on a non-staggered grid; unsupported")

    if (Knowledge.grid_isAxisAligned) // includes uniform grids
      (0 until domain.numDims).map(dim => L4_VF_StagCellWidthPerDim.access(level, stagDim, dim, Duplicate(index)) : L4_Expression).reduce(_ * _)
    else
      Logger.error("Currently unsupported")
  }

  override def progressImpl() = IR_VF_StagCellVolume(level, domain.getProgressedObj(), stagDim)
}
