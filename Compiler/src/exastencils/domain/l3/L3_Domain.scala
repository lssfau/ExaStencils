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

import exastencils.config.Knowledge
import exastencils.domain.l4.L4_Domain
import exastencils.knowledge.l3.L3_KnowledgeObject
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_Domain

abstract class L3_Domain extends L3_KnowledgeObject[L4_Domain] {
  def numDims : Int

//  override def prettyprintDecl(out : PpStream) : Unit = ???
//  override def progressImpl() = L4_Domain(name)

  override def createDuplicate() : L3_KnowledgeObject[L4_Domain] = Logger.error("Trying to duplicate an l3 domain. This is currently unsupported.")
}

/// L3_DummyDomain

case class L3_DummyDomain() extends L3_Domain {
  override def name = "dummy"
  override def numDims = Knowledge.dimensionality
  override def prettyprintDecl(out : PpStream) = Logger.error("Trying to print l3 dummy domain; unsupported")
  override def progressImpl() = Logger.error("Trying to progress l3 dummy domain; unsupported")
}
