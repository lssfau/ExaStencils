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

package exastencils.field.ir

import scala.collection.mutable.ListBuffer

import exastencils.core.Duplicate
import exastencils.knowledge.ir._

/// IR_FieldCombination

case class IR_FieldCombination(
    var name : String,
    var level : Int,
    var combinationType : String,
    var fields : ListBuffer[IR_Field]) extends IR_LeveledKnowledgeObject {

  override def createDuplicate() : IR_FieldCombination = {
    IR_FieldCombination.tupled(Duplicate(IR_FieldCombination.unapply(this).get))
  }
}
