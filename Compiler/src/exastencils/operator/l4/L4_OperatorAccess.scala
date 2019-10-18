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

package exastencils.operator.l4

import scala.collection.mutable._

import exastencils.base.l4._
import exastencils.knowledge.l4.L4_LeveledKnowledgeAccess
import exastencils.operator.ir.IR_OperatorAccess

/// L4_OperatorAccess

abstract class L4_OperatorAccess extends L4_LeveledKnowledgeAccess {
  override def progress : IR_OperatorAccess
  def assembleOffsetMap() : Map[L4_Expression, ListBuffer[L4_ConstIndex]]
}
