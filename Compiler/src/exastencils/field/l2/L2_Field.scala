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
import exastencils.boundary.l2._
import exastencils.core.Duplicate
import exastencils.domain.l2.L2_Domain
import exastencils.field.l3.L3_Field
import exastencils.fieldlike.l2.L2_FieldLike
import exastencils.grid.l2.L2_Localization
import exastencils.prettyprinting.PpStream

/// L2_Field

object L2_Field {
  def apply(name : String, level : Int, domain : L2_Domain, datatype : L2_Datatype, localization : L2_Localization,
      initial : Option[L2_Expression], boundary : L2_BoundaryCondition) =
    new L2_Field(name, level, domain, datatype, localization, 1, initial, boundary)
}

case class L2_Field(
    var name : String,
    var level : Int,
    var domain : L2_Domain,
    var datatype : L2_Datatype,
    var localization : L2_Localization,
    var numSlots : Int,
    var initial : Option[L2_Expression],
    var boundary : L2_BoundaryCondition) extends L2_FieldLike[L3_Field] {

  override def createDuplicate() : L2_Field = {
    L2_Field(name, level, Duplicate(domain), Duplicate(datatype), Duplicate(localization), numSlots, Duplicate(initial), Duplicate(boundary))
  }

  override def prettyprintDecl(out : PpStream) : Unit = {
    out << "Field " << name << "@" << level << " with " << datatype << " on " << localization << " of " << domain.name
    if (numSlots > 1) out << " " << numSlots << " times "
    if (initial.isDefined) out << " = " << initial.get

    if (boundary != L2_NoBC) {
      out << "\n"
      out << "Field " << name << "@" << level << " on boundary = " << boundary
    }
  }

  override def progressImpl() = {
    L3_Field(
      name,
      level,
      domain.getProgressedObj(),
      datatype.progress,
      localization.progress,
      numSlots,
      L2_ProgressOption(initial)(_.progress),
      boundary.progress)
  }
}
