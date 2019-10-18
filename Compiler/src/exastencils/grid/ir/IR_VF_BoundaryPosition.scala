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
import exastencils.domain.ir.IR_Domain
import exastencils.logger.Logger

/// IR_VF_BoundaryPositionAsVec

object IR_VF_BoundaryPositionAsVec {
  def find(level : Int) = IR_VirtualField.findVirtualField(s"vf_boundaryPosition", level).asInstanceOf[IR_VF_BoundaryPositionAsVec]
  def access(level : Int, index : IR_ExpressionIndex) = IR_VirtualFieldAccess(find(level), index)
}

case class IR_VF_BoundaryPositionAsVec(
    var level : Int,
    var domain : IR_Domain
) extends IR_VirtualFieldWithVec {

  override def name = "vf_boundaryPosition"
  override def knownAliases = ListBuffer("vf_boundaryCoordinateAsVec", "vf_boundaryCoordAsVec", "vf_boundaryPositionAsVec", "vf_boundaryPosAsVec", "vf_boundaryPos")
  override def localization = IR_AtBoundary
  override def resolutionPossible = true

  override def createDuplicate() = IR_VF_BoundaryPositionAsVec(level, domain)

  override def listPerDim = (0 until numDims).map(IR_VF_BoundaryPositionPerDim.find(level, _) : IR_VirtualField).to[ListBuffer]
}

/// IR_VF_BoundaryPositionPerDim

object IR_VF_BoundaryPositionPerDim {
  def find(level : Int, dim : Int) = IR_VirtualField.findVirtualField(s"vf_boundaryPosition_$dim", level).asInstanceOf[IR_VF_BoundaryPositionPerDim]
  def access(level : Int, dim : Int, index : IR_ExpressionIndex) = IR_VirtualFieldAccess(find(level, dim), index)
}

case class IR_VF_BoundaryPositionPerDim(
    var level : Int,
    var domain : IR_Domain,
    var dim : Int
) extends IR_VirtualFieldPerDim {

  override def name = s"vf_boundaryPosition_$dim"
  override def knownAliases = {
    ListBuffer(dim.toString, IR_Localization.dimToString(dim)).flatMap(postfix =>
      ListBuffer(s"vf_boundaryPos_$postfix", s"vf_boundaryCoordinate_$postfix", s"vf_boundaryCoord_$postfix") :+
        s"vf_boundaryPosition_${ IR_Localization.dimToString(dim) }"
    )
  }
  override def localization = IR_AtBoundary
  override def resolutionPossible = false

  override def createDuplicate() = IR_VF_BoundaryPositionPerDim(level, domain, dim)

  override def resolve(index : IR_ExpressionIndex) = Logger.error("Trying to resolve boundary position; unsupported")
}
