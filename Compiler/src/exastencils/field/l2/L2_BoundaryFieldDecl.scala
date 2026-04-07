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
import exastencils.fieldlike.l2.L2_FieldLikeOnBoundaryDecl
import exastencils.prettyprinting._

/// L2_BoundaryFieldDecl

case class L2_BoundaryFieldDecl(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var boundary : L2_BoundaryCondition) extends L2_FieldLikeOnBoundaryDecl {

  override def prettyprint(out : PpStream) = {
    out << "Field " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " on boundary = " << boundary
  }

  def addToKnowledge() : Unit = {
    val fieldToAdapt = L2_FieldCollection.getByIdentifier(name, L2_LevelSpecification.asSingleLevel(levels)).get
    fieldToAdapt.boundary = boundary
  }
}
