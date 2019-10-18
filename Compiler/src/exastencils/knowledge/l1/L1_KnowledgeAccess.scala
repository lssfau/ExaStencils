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

package exastencils.knowledge.l1

import exastencils.base.l1.L1_Access

/// L1_KnowledgeAccess

trait L1_KnowledgeAccess extends L1_Access {
  def target : L1_KnowledgeObject[_]
  override def name : String = target.name
}

/// L1_LeveledKnowledgeAccess

trait L1_LeveledKnowledgeAccess extends L1_KnowledgeAccess {
  override def target : L1_LeveledKnowledgeObject[_]
  override def name : String = target.name
  def level : Int = target.level
}
