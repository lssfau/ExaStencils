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

package exastencils.knowledge.ir

import exastencils.base.ir.IR_Access

/// IR_KnowledgeAccess

trait IR_KnowledgeAccess extends IR_Access {
  def target : IR_KnowledgeObject
  /*override*/ def name : String = target.name
}

/// IR_LeveledKnowledgeAccess

trait IR_LeveledKnowledgeAccess extends IR_KnowledgeAccess {
  override def target : IR_LeveledKnowledgeObject
  override def name : String = target.name
  def level : Int = target.level
}
