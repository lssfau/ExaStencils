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

import exastencils.base.l2._
import exastencils.domain.l2.L2_Domain
import exastencils.grid.l3._
import exastencils.logger.Logger

/// L2_VF_BoundaryPositionAsVec

object L2_VF_BoundaryPositionAsVec {
  def find(level : Int) = L2_VirtualField.findVirtualField(s"vf_boundaryPosition", level).asInstanceOf[L2_VF_BoundaryPositionAsVec]
  def access(level : Int, index : L2_ExpressionIndex) = L2_VirtualFieldAccess(find(level), index)
}

case class L2_VF_BoundaryPositionAsVec(
    var level : Int,
    var domain : L2_Domain
) extends L2_VirtualFieldWithVec {

  override def name = "vf_boundaryPosition"
  override def knownAliases = ListBuffer("vf_boundaryCoordinateAsVec", "vf_boundaryCoordAsVec", "vf_boundaryPositionAsVec", "vf_boundaryPosAsVec", "vf_boundaryPos")
  override def localization = L2_AtBoundary
  override def resolutionPossible = true

  override def createDuplicate() = L2_VF_BoundaryPositionAsVec(level, domain)

  override def listPerDim = (0 until numDims).map(L2_VF_BoundaryPositionPerDim.find(level, _) : L2_VirtualField).to[ListBuffer]
  override def progressImpl() = L3_VF_BoundaryPositionAsVec(level, domain.getProgressedObj())
}

/// L2_VF_BoundaryPositionPerDim

object L2_VF_BoundaryPositionPerDim {
  def find(level : Int, dim : Int) = L2_VirtualField.findVirtualField(s"vf_boundaryPosition_$dim", level).asInstanceOf[L2_VF_BoundaryPositionPerDim]
  def access(level : Int, dim : Int, index : L2_ExpressionIndex) = L2_VirtualFieldAccess(find(level, dim), index)
}

case class L2_VF_BoundaryPositionPerDim(
    var level : Int,
    var domain : L2_Domain,
    var dim : Int
) extends L2_VirtualFieldPerDim {

  override def name = s"vf_boundaryPosition_$dim"
  override def knownAliases = {
    ListBuffer(dim.toString, L2_Localization.dimToString(dim)).flatMap(postfix =>
      ListBuffer(s"vf_boundaryPos_$postfix", s"vf_boundaryCoordinate_$postfix", s"vf_boundaryCoord_$postfix") :+
        s"vf_boundaryPosition_${ L2_Localization.dimToString(dim) }"
    )
  }
  override def localization = L2_AtBoundary
  override def resolutionPossible = false

  override def createDuplicate() = L2_VF_BoundaryPositionPerDim(level, domain, dim)

  override def resolve(index : L2_ExpressionIndex) = Logger.error("Trying to resolve boundary position; unsupported")
  override def progressImpl() = L3_VF_BoundaryPositionPerDim(level, domain.getProgressedObj(), dim)
}
