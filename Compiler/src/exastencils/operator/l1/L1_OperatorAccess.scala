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

package exastencils.operator.l1

import exastencils.base.ProgressLocation
import exastencils.datastructures._
import exastencils.knowledge.l1.L1_LeveledKnowledgeAccess
import exastencils.operator.l2.L2_StencilAccess
import exastencils.prettyprinting.PpStream

/// L1_OperatorAccess

object L1_OperatorAccess {
  def apply(access : L1_FutureOperatorAccess) =
    new L1_OperatorAccess(L1_OperatorCollection.getByIdentifier(access.name, access.level).get)
}

case class L1_OperatorAccess(var target : L1_Operator) extends L1_LeveledKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << name << '@' << level
  override def progress = ProgressLocation(L2_StencilAccess(target.getProgressedObj(), None, None))
}

/// L1_ResolveOperatorAccesses

object L1_ResolveOperatorAccesses extends DefaultStrategy("Resolve accesses to operators") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L1_FutureOperatorAccess if L1_OperatorCollection.exists(access.name, access.level) =>
      access.toOperatorAccess
  })
}
