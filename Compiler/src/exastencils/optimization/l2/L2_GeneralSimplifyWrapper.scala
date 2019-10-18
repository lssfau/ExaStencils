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

package exastencils.optimization.l2

import exastencils.base.l2._
import exastencils.logger.Logger

/// L2_GeneralSimplifyWrapper

object L2_GeneralSimplifyWrapper {
  def process[T <: L2_Node](node : T) : T = {
    val wrapped = L2_Root(node)
    L2_GeneralSimplify.doUntilDoneStandalone(wrapped)
    if (wrapped.nodes.length != 1) Logger.warn(s"L2_GeneralSimplify changed number of nodes on $node")
    wrapped.nodes.head.asInstanceOf[T]
  }
}
