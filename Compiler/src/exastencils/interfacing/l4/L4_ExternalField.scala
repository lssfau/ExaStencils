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

package exastencils.interfacing.l4

import exastencils.field.l4._
import exastencils.interfacing.ir.IR_ExternalField
import exastencils.knowledge.l4.L4_LeveledKnowledgeObject
import exastencils.prettyprinting._

/// L4_ExternalField

case class L4_ExternalField(
    var name : String, // will be used to find the field
    var level : Int, // the level the field lives on
    var fieldLayout : L4_FieldLayout,
    var targetField : L4_Field // the (internal) field to be copied to/ from
) extends L4_LeveledKnowledgeObject[IR_ExternalField] {

  override def prettyprintDecl(out : PpStream) = {
    out << "external Field " << name << " <" << fieldLayout << "> => " << targetField << "@" << level
  }

  override def createDuplicate() = L4_ExternalField(name, level, fieldLayout, targetField)

  override def progressImpl() = IR_ExternalField(name, targetField.getProgressedObj(), fieldLayout.getProgressedObj(), level)
}
