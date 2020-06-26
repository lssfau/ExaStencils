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

package exastencils.field.l4

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_MatStructure
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.boundary.l4._
import exastencils.domain.l4._
import exastencils.prettyprinting._

/// L4_BaseFieldDecl

object L4_BaseFieldDecl {
  def apply(name : String, levels : Option[L4_DeclarationLevelSpecification], domainName : String, fieldLayoutName : String, boundary : L4_BoundaryCondition, numSlots : Integer) =
    new L4_BaseFieldDecl(name, levels, L4_UnresolvedAccess(domainName), L4_UnresolvedAccess(fieldLayoutName), boundary, numSlots)
}

case class L4_BaseFieldDecl(
    var name : String,
    var levels : Option[L4_DeclarationLevelSpecification],
    var domain : L4_Access,
    var fieldLayout : L4_Access,
    var boundary : L4_BoundaryCondition,
    var numSlots : Integer,
    var matStructure : Option[L4_MatStructure] = None
) extends L4_FieldDecl {

  override def prettyprint(out : PpStream) = {
    out << "Field " << name << "< " << domain.name << ", " << fieldLayout.name << ", " << boundary
    if (matStructure.isDefined) out << ", " << matStructure
    out << ">"
    if (numSlots > 1) out << '[' << numSlots << ']'
    if (levels.isDefined) out << '@' << levels.get
  }

  override def addToKnowledge() : Unit = {
    val index = L4_FieldDecl.runningIndex
    L4_FieldDecl.runningIndex += 1

    L4_FieldCollection.add(
      L4_Field(name, levels.get.resolveLevel,
        index,
        domain.asInstanceOf[L4_DomainAccess].target,
        fieldLayout.asInstanceOf[L4_FieldLayoutAccess].target,
        numSlots,
        boundary,
        matStructure
      ))
  }
}
