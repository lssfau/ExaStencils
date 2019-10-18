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
import exastencils.domain.l3.L3_Domain
import exastencils.grid.l4._
import exastencils.logger.Logger

/// L3_VF_NodePositionAsVec

object L3_VF_NodePositionAsVec {
  def find(level : Int) = L3_VirtualField.findVirtualField(s"vf_nodePosition", level).asInstanceOf[L3_VF_NodePositionAsVec]
  def access(level : Int, index : L3_ExpressionIndex) = L3_VirtualFieldAccess(find(level), index)
}

case class L3_VF_NodePositionAsVec(
    var level : Int,
    var domain : L3_Domain
) extends L3_VirtualFieldWithVec {

  override def name = "vf_nodePosition"
  override def knownAliases = ListBuffer("vf_nodePositionAsVec", "vf_nodePosAsVec", "vf_nodePos")
  override def localization = L3_AtNode
  override def resolutionPossible = true

  override def createDuplicate() = L3_VF_NodePositionAsVec(level, domain)

  override def listPerDim = (0 until numDims).map(L3_VF_NodePositionPerDim.find(level, _) : L3_VirtualField).to[ListBuffer]

  override def progressImpl() = L4_VF_NodePositionAsVec(level, domain.getProgressedObj())
}

/// L3_VF_NodePositionPerDim

object L3_VF_NodePositionPerDim {
  def find(level : Int, dim : Int) = L3_VirtualField.findVirtualField(s"vf_nodePosition_$dim", level).asInstanceOf[L3_VF_NodePositionPerDim]
  def access(level : Int, dim : Int, index : L3_ExpressionIndex) = L3_VirtualFieldAccess(find(level, dim), index)
}

case class L3_VF_NodePositionPerDim(
    var level : Int,
    var domain : L3_Domain,
    var dim : Int
) extends L3_VirtualFieldPerDim {

  override def name = s"vf_nodePosition_$dim"
  override def knownAliases = ListBuffer(s"vf_nodePosition_${ L3_Localization.dimToString(dim) }", s"vf_nodePos_$dim", s"vf_nodePos_${ L3_Localization.dimToString(dim) }")
  override def localization = L3_AtNode
  override def resolutionPossible = false

  override def createDuplicate() = L3_VF_NodePositionPerDim(level, domain, dim)

  override def resolve(index : L3_ExpressionIndex) = Logger.error("Trying to resolve node position; unsupported")

  override def progressImpl() = L4_VF_NodePositionPerDim(level, domain.getProgressedObj(), dim)
}
