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
import exastencils.boundary.l3._
import exastencils.fieldlike.l3.L3_FieldLikeOnBoundaryDecl
import exastencils.prettyprinting._

/// L3_BoundaryFieldDecl

case class L3_BoundaryFieldDecl(
    var name : String,
    var levels : Option[L3_LevelSpecification],
    var boundary : L3_BoundaryCondition) extends L3_FieldLikeOnBoundaryDecl {

  override def prettyprint(out : PpStream) = {
    out << "Field " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " on boundary = " << boundary
  }

  def addToKnowledge() : Unit = {
    val fieldToAdapt = L3_FieldCollection.getByIdentifier(name, L3_LevelSpecification.asSingleLevel(levels)).get
    fieldToAdapt.boundary = boundary
  }
}
