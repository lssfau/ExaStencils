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

package exastencils.domain.ir

import exastencils.knowledge.ir.IR_KnowledgeObject
import exastencils.logger.Logger

/// IR_Domain

object IR_Domain {
  var runningIndex = 0
}

trait IR_Domain extends IR_KnowledgeObject {
  def numDims : Int
  def index : Int
  def HACK_shape : Any

  override def createDuplicate() : IR_KnowledgeObject = Logger.error("Trying to duplicate an ir domain. This is currently unsupported.")
}
