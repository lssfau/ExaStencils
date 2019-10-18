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

package exastencils.operator.l3

import scala.collection.mutable._

import exastencils.base.l3._
import exastencils.knowledge.l3.L3_LeveledKnowledgeAccess
import exastencils.operator.l4.L4_OperatorAccess

/// L3_OperatorAccess

abstract class L3_OperatorAccess extends L3_LeveledKnowledgeAccess {
  override def progress : L4_OperatorAccess
  def assembleOffsetMap() : Map[L3_Expression, ListBuffer[L3_ConstIndex]]
}
