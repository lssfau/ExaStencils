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
import exastencils.fieldlike.l3.L3_FieldLikeFromOther
import exastencils.prettyprinting._

/// L3_FieldFromOther

case class L3_FieldFromOther(var name : String, var levels : Option[L3_LevelSpecification], var src : L3_Access) extends L3_FieldLikeFromOther[L3_Field] {

  override def prettyprint(out : PpStream) = {
    out << "Field" << ' ' << name
    if (levels.isDefined) out << '@' << levels.get
    out << " from" << ' ' << src
  }

  override def addToKnowledge() : Unit = {
    val destField = src.asInstanceOf[L3_FieldAccess].target.createDuplicate()
    destField.name = name
    L3_FieldCollection.add(destField)
  }
}
