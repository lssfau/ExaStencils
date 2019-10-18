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

package exastencils.domain.l4

import exastencils.datastructures._
import exastencils.knowledge.l4._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_DomainAccess

object L4_DomainAccess {
  def apply(name : String) =
    new L4_DomainAccess(L4_DomainCollection.getByIdentifier(name).get)

  def apply(access : L4_FutureDomainAccess) =
    new L4_DomainAccess(L4_DomainCollection.getByIdentifier(access.name).get)
}

case class L4_DomainAccess(var target : L4_Domain) extends L4_KnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name
  override def progress = Logger.error(s"Trying to progress access to domain ${ target.name } - unsupported")
}

/// L4_ResolveDomainAccesses

object L4_ResolveDomainAccesses extends DefaultStrategy("Resolve accesses to domains") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L4_FutureDomainAccess if L4_DomainCollection.exists(access.name) =>
      access.toDomainAccess
  })
}
