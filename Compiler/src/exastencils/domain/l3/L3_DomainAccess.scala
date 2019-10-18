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

package exastencils.domain.l3

import exastencils.base.ProgressLocation
import exastencils.datastructures._
import exastencils.domain.l4.L4_DomainAccess
import exastencils.knowledge.l3._
import exastencils.prettyprinting.PpStream

/// L3_DomainAccess

object L3_DomainAccess {
  def apply(name : String) =
    new L3_DomainAccess(L3_DomainCollection.getByIdentifier(name).get)

  def apply(access : L3_FutureDomainAccess) =
    new L3_DomainAccess(L3_DomainCollection.getByIdentifier(access.name).get)
}

case class L3_DomainAccess(var target : L3_Domain) extends L3_KnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name
  override def progress = ProgressLocation(L4_DomainAccess(target.getProgressedObj()))
}

/// L3_ResolveDomainAccesses

object L3_ResolveDomainAccesses extends DefaultStrategy("Resolve accesses to domains") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L3_FutureDomainAccess if L3_DomainCollection.exists(access.name) =>
      access.toDomainAccess
  })
}
