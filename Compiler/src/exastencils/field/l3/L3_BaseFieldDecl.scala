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

package exastencils.field.l3

import exastencils.base.l3._
import exastencils.boundary.l3.L3_NoBC
import exastencils.domain.l3._
import exastencils.field.l4.L4_Field
import exastencils.fieldlike.l3.L3_FieldLikeCollection
import exastencils.grid.l3.L3_Localization
import exastencils.prettyprinting._

/// L3_BaseFieldDecl

object L3_BaseFieldDecl {
  def apply(identifier : String, levels : Option[L3_LevelSpecification], datatype : Option[L3_Datatype], localization : String, domain : String, numSlots : Option[Int], initial : Option[L3_Expression]) : L3_BaseFieldDecl =
    L3_BaseFieldDecl(identifier, levels, datatype.getOrElse(L3_RealDatatype), L3_Localization.resolve(localization), L3_FutureDomainAccess(domain), numSlots, initial)
}

case class L3_BaseFieldDecl(
    var name : String,
    var levels : Option[L3_LevelSpecification],
    var datatype : L3_Datatype,
    var localization : L3_Localization,
    var domain : L3_Access,
    var numSlots : Option[Int],
    var initial : Option[L3_Expression]) extends L3_FieldDecl {

  override def prettyprint(out : PpStream) = {
    out << "Field " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " with " << datatype << " on " << localization << " of " << domain
    if (numSlots.isDefined) out << " " << numSlots.get << " times"
    if (initial.isDefined) out << " = " << initial.get
  }

  override def addToKnowledge() : Unit = {
    L3_FieldCollection.add(
      L3_Field(
        name,
        L3_LevelSpecification.asSingleLevel(levels),
        domain.asInstanceOf[L3_DomainAccess].target,
        datatype,
        localization,
        numSlots.getOrElse(1),
        initial,
        L3_NoBC))
  }

  override def associatedCollection : L3_FieldLikeCollection[L3_Field, L4_Field] = L3_FieldCollection
}
