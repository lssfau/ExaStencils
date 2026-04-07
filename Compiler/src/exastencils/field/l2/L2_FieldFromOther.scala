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
import exastencils.field.l3.L3_Field
import exastencils.fieldlike.l2.L2_FieldLikeCollection
import exastencils.prettyprinting._

/// L2_FieldFromOther

case class L2_FieldFromOther(var name : String, var levels : Option[L2_LevelSpecification], var src : L2_Access) extends L2_FieldDecl {

  override def prettyprint(out : PpStream) = {
    out << "Field" << ' ' << name
    if (levels.isDefined) out << '@' << levels.get
    out << " from" << ' ' << src
  }

  override def addToKnowledge() : Unit = {
    val destField = src.asInstanceOf[L2_FieldAccess].target.createDuplicate()
    destField.name = name
    L2_FieldCollection.add(destField)
  }

  override def associatedCollection : L2_FieldLikeCollection[L2_Field, L3_Field] = L2_FieldCollection
}
