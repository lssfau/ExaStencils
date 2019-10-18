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

package exastencils.operator.ir

import exastencils.base.ir._
import exastencils.field.ir.IR_Field
import exastencils.knowledge.ir._

/// IR_StencilField

case class IR_StencilField(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var stencil : IR_Stencil, // linked stencil
    var field : IR_Field // linked coefficient field
) extends IR_LeveledKnowledgeObject {

  override def createDuplicate() = IR_StencilField(name, level, stencil, field)

  def findStencilEntryIndex(offset : IR_ConstIndex) : Option[Int] = stencil.findStencilEntryIndex(offset)
}
