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

package exastencils.domain.ir

import exastencils.config.Knowledge
import exastencils.util.ir.IR_AABB

/// IR_DomainFromAABB

case class IR_DomainFromAABB(var name : String, aabb : IR_AABB) extends IR_Domain {
  var index = IR_Domain.runningIndex
  IR_Domain.runningIndex += 1

  /// HACK: make this information accessible in some other way
  if (Knowledge.domain_rect_generate && "global" == name) {
    Knowledge.discr_hx = (Knowledge.minLevel to Knowledge.maxLevel).toArray.map(
      level => aabb.width(0) / (Knowledge.domain_rect_numFragsTotal_x * Knowledge.domain_fragmentLength_x * (1 << level)))
    if (Knowledge.dimensionality > 1)
      Knowledge.discr_hy = (Knowledge.minLevel to Knowledge.maxLevel).toArray.map(
        level => aabb.width(1) / (Knowledge.domain_rect_numFragsTotal_y * Knowledge.domain_fragmentLength_y * (1 << level)))
    if (Knowledge.dimensionality > 2)
      Knowledge.discr_hz = (Knowledge.minLevel to Knowledge.maxLevel).toArray.map(
        level => aabb.width(2) / (Knowledge.domain_rect_numFragsTotal_z * Knowledge.domain_fragmentLength_z * (1 << level)))
  }

  override def HACK_shape : Any = ???
  override def numDims = aabb.numDims
}
