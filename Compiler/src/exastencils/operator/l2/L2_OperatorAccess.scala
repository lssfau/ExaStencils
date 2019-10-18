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

package exastencils.operator.l2

import scala.collection.mutable._

import exastencils.base.l2._
import exastencils.knowledge.l2.L2_LeveledKnowledgeAccess
import exastencils.operator.l3.L3_OperatorAccess

/// L2_OperatorAccess

abstract class L2_OperatorAccess extends L2_LeveledKnowledgeAccess {
  override def progress : L3_OperatorAccess
  def assembleOffsetMap() : Map[L2_Expression, ListBuffer[L2_ConstIndex]]
}
