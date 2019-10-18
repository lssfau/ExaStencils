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

package exastencils.domain.l2

import exastencils.base.ProgressLocation
import exastencils.datastructures._
import exastencils.domain.l3.L3_DomainAccess
import exastencils.knowledge.l2._
import exastencils.prettyprinting.PpStream

/// L2_DomainAccess

object L2_DomainAccess {
  def apply(name : String) =
    new L2_DomainAccess(L2_DomainCollection.getByIdentifier(name).get)

  def apply(access : L2_FutureDomainAccess) =
    new L2_DomainAccess(L2_DomainCollection.getByIdentifier(access.name).get)
}

case class L2_DomainAccess(var target : L2_Domain) extends L2_KnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name
  override def progress = ProgressLocation(L3_DomainAccess(target.getProgressedObj()))
}

/// L2_ResolveDomainAccesses

object L2_ResolveDomainAccesses extends DefaultStrategy("Resolve accesses to domains") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L2_FutureDomainAccess if L2_DomainCollection.exists(access.name) =>
      access.toDomainAccess
  })
}
