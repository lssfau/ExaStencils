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

package exastencils.domain.l1

import exastencils.base.ProgressLocation
import exastencils.datastructures._
import exastencils.domain.l2.L2_DomainAccess
import exastencils.knowledge.l1._
import exastencils.prettyprinting.PpStream

/// L1_DomainAccess

object L1_DomainAccess {
  def apply(name : String) =
    new L1_DomainAccess(L1_DomainCollection.getByIdentifier(name).get)

  def apply(access : L1_FutureDomainAccess) =
    new L1_DomainAccess(L1_DomainCollection.getByIdentifier(access.name).get)
}

case class L1_DomainAccess(var target : L1_Domain) extends L1_KnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name
  override def progress = ProgressLocation(L2_DomainAccess(target.getProgressedObj()))
}

/// L1_ResolveDomainAccesses

object L1_ResolveDomainAccesses extends DefaultStrategy("Resolve accesses to domains") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L1_FutureDomainAccess if L1_DomainCollection.exists(access.name) =>
      access.toDomainAccess
  })
}
