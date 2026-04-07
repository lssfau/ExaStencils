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
import exastencils.boundary.l3.L3_BoundaryCondition
import exastencils.core.Duplicate
import exastencils.domain.l3.L3_Domain
import exastencils.field.l4._
import exastencils.fieldlike.l3.L3_FieldLike
import exastencils.grid.l3.L3_Localization
import exastencils.prettyprinting.PpStream

/// L3_Field

object L3_Field {
  def apply(name : String, level : Int, domain : L3_Domain, datatype : L3_Datatype, localization : L3_Localization,
      initial : Option[L3_Expression], boundary : L3_BoundaryCondition) =
    new L3_Field(name, level, domain, datatype, localization, 1, initial, boundary)
}

case class L3_Field(
    var name : String,
    var level : Int,
    var domain : L3_Domain,
    var datatype : L3_Datatype,
    var localization : L3_Localization,
    var numSlots : Int,
    var initial : Option[L3_Expression],
    var boundary : L3_BoundaryCondition) extends L3_FieldLike[L4_Field] {

  override def createDuplicate() : L3_Field = {
    L3_Field(name, level, Duplicate(domain), Duplicate(datatype), Duplicate(localization), numSlots, Duplicate(initial), Duplicate(boundary))
  }

  def fieldLayoutName = s"defLayoutFor_${ printDatatype(datatype) }_on_${ localization.prettyprint() }"

  override def prettyprintDecl(out : PpStream) : Unit = {
    out << "Field " << name << "@" << level << " with " << datatype << " on " << localization << " of " << domain.name
    if (numSlots > 1) out << " " << numSlots << " times "
    if (initial.isDefined) out << " = " << initial.get
    out << "\n\n"

    out << "Field " << name << "@" << level << " on boundary = " << boundary
  }

  override def progressImpl() = {
    L4_Field(
      name,
      level,
      -1, // index is to be set later
      domain.getProgressedObj(),
      L4_FieldLayoutCollection.getByIdentifier(fieldLayoutName, level).get, // l3 field layout is not available -> grab l4 layout directly
      numSlots,
      boundary.progress)
  }

  override def toField : L3_Field = this
}
