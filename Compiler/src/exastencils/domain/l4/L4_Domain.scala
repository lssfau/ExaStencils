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

import exastencils.config.Knowledge
import exastencils.domain.ir.IR_Domain
import exastencils.knowledge.l4.L4_KnowledgeObject
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_Domain

abstract class L4_Domain extends L4_KnowledgeObject[IR_Domain] {
  def numDims : Int

//  override def prettyprintDecl(out : PpStream) : Unit = ???
//  override def progressImpl() = IR_Domain(name)

  override def createDuplicate() : L4_KnowledgeObject[IR_Domain] = Logger.error("Trying to duplicate an l4 domain. This is currently unsupported.")
}

/// L4_DummyDomain

case class L4_DummyDomain() extends L4_Domain {
  override def name = "dummy"
  override def numDims = Knowledge.dimensionality
  override def prettyprintDecl(out : PpStream) = Logger.error("Trying to print l4 dummy domain; unsupported")
  override def progressImpl() = Logger.error("Trying to progress l4 dummy domain; unsupported")
}
