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

import exastencils.baseExt.l2.L2_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l2.L2_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l2.L2_LevelCollector

/// L2_FutureDomainAccess

case class L2_FutureDomainAccess(var name : String) extends L2_FutureKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << name
  override def progress = Logger.error(s"Trying to progress future domain access to $name")
  def toDomainAccess = L2_DomainAccess(this)
}

/// L2_PrepareDomainAccesses

object L2_PrepareDomainAccesses extends DefaultStrategy("Prepare accesses to domains") {
  val collector = new L2_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L2_UnresolvedAccess if L2_DomainCollection.existsDecl(access.name) =>
      if (!L2_DomainCollection.existsDecl(access.name))
        Logger.warn(s"Trying to access ${ access.name }")

      if (access.level.isDefined)
        Logger.warn(s"Ignoring invalid level specification in access to domain ${ access.name }")

      L2_FutureDomainAccess(access.name)
  })
}

