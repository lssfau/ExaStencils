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

package exastencils.knowledge.l2

import exastencils.base.l2.L2_Access

/// L2_KnowledgeAccess

trait L2_KnowledgeAccess extends L2_Access {
  def target : L2_KnowledgeObject[_]
  override def name : String = target.name
}

/// L2_LeveledKnowledgeAccess

trait L2_LeveledKnowledgeAccess extends L2_KnowledgeAccess {
  override def target : L2_LeveledKnowledgeObject[_]
  override def name : String = target.name
  def level : Int = target.level
}
