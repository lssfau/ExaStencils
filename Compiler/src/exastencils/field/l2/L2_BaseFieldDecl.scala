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

package exastencils.field.l2

import exastencils.base.l2._
import exastencils.boundary.l2.L2_NoBC
import exastencils.domain.l2._
import exastencils.field.l3.L3_Field
import exastencils.fieldlike.l2.L2_FieldLikeCollection
import exastencils.grid.l2.L2_Localization
import exastencils.prettyprinting._

/// L2_BaseFieldDecl

object L2_BaseFieldDecl {
  def apply(identifier : String, levels : Option[L2_LevelSpecification], datatype : Option[L2_Datatype], localization : String, domain : String, numSlots : Option[Int], initial : Option[L2_Expression]) : L2_BaseFieldDecl =
    L2_BaseFieldDecl(identifier, levels, datatype.getOrElse(L2_RealDatatype), L2_Localization.resolve(localization), L2_FutureDomainAccess(domain), numSlots, initial)
}

case class L2_BaseFieldDecl(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var datatype : L2_Datatype,
    var localization : L2_Localization,
    var domain : L2_Access,
    var numSlots : Option[Int],
    var initial : Option[L2_Expression]) extends L2_FieldDecl {

  override def prettyprint(out : PpStream) = {
    out << "Field " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " with " << datatype << " on " << localization << " of " << domain
    if (numSlots.isDefined) out << " " << numSlots.get << " times"
    if (initial.isDefined) out << " = " << initial.get
  }

  override def addToKnowledge() : Unit = {
    L2_FieldCollection.add(
      L2_Field(
        name,
        L2_LevelSpecification.asSingleLevel(levels),
        domain.asInstanceOf[L2_DomainAccess].target,
        datatype,
        localization,
        numSlots.getOrElse(1),
        initial,
        L2_NoBC))
  }

  override def associatedCollection : L2_FieldLikeCollection[L2_Field, L3_Field] = L2_FieldCollection
}
