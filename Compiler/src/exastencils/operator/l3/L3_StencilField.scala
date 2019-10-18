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

import exastencils.field.l3.L3_Field
import exastencils.knowledge.l3._
import exastencils.operator.l4.L4_StencilField
import exastencils.prettyprinting.PpStream

/// L3_StencilField

case class L3_StencilField(
    var name : String, // will be used to find the stencil
    var level : Int, // the level the stencil lives on
    var stencil : L3_Stencil, // linked stencil
    var field : L3_Field // linked coefficient field
) extends L3_LeveledKnowledgeObject[L4_StencilField] {

  override def createDuplicate() = L3_StencilField(name, level, stencil, field)

  override def prettyprintDecl(out : PpStream) : Unit = {
    out << "Operator " << name << "@" << level << " from StencilTemplate on " << field.localization << " of " << field.domain.name << " {\n"
    stencil.entries.foreach(out << _.asStencilOffsetEntry.offset << " => \n")
    out << "}"
  }

  override def progressImpl() = L4_StencilField(name, level, stencil.getProgressedObj(), field.getProgressedObj())
}
