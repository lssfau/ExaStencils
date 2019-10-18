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

import exastencils.config.Knowledge
import exastencils.domain.l2.L2_Domain
import exastencils.knowledge.l1.L1_KnowledgeObject
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L1_Domain

abstract class L1_Domain extends L1_KnowledgeObject[L2_Domain] {
  var name : String
  def numDims : Int

  override def createDuplicate() : L1_KnowledgeObject[L2_Domain] = Logger.error("Trying to duplicate an l1 domain. This is currently unsupported.")
}

/// L1_DummyDomain

case class L1_DummyDomain() extends L1_Domain {
  override var name = "dummy"
  override def numDims = Knowledge.dimensionality
  override def prettyprintDecl(out : PpStream) = Logger.error("Trying to print l1 dummy domain; unsupported")
  override def progressImpl() = Logger.error("Trying to progress l1 dummy domain; unsupported")
}
