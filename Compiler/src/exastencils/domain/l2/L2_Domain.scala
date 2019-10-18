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

import exastencils.config.Knowledge
import exastencils.domain.l3.L3_Domain
import exastencils.knowledge.l2.L2_KnowledgeObject
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_Domain

abstract class L2_Domain extends L2_KnowledgeObject[L3_Domain] {
  def numDims : Int

//  override def prettyprintDecl(out : PpStream) : Unit = ???
//  override def progressImpl() = L3_Domain(name)

  override def createDuplicate() : L2_KnowledgeObject[L3_Domain] = Logger.error("Trying to duplicate an l2 domain. This is currently unsupported.")
}

/// L2_DummyDomain

case class L2_DummyDomain() extends L2_Domain {
  override def name = "dummy"
  override def numDims = Knowledge.dimensionality
  override def prettyprintDecl(out : PpStream) = Logger.error("Trying to print l2 dummy domain; unsupported")
  override def progressImpl() = Logger.error("Trying to progress l2 dummy domain; unsupported")
}
