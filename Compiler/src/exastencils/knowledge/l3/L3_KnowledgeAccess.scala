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

package exastencils.knowledge.l3

import exastencils.base.l3.L3_Access

/// L3_KnowledgeAccess

trait L3_KnowledgeAccess extends L3_Access {
  def target : L3_KnowledgeObject[_]
  override def name : String = target.name
}

/// L3_LeveledKnowledgeAccess

trait L3_LeveledKnowledgeAccess extends L3_KnowledgeAccess {
  override def target : L3_LeveledKnowledgeObject[_]
  override def name : String = target.name
  def level : Int = target.level
}
