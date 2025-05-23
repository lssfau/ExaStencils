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

import exastencils.field.l4.L4_Field
import exastencils.fieldlike.l3.L3_FieldLikeDecl
import exastencils.logger._

/// L3_FieldDecl

abstract class L3_FieldDecl extends L3_FieldLikeDecl[L3_Field, L4_Field] {
  override def progress = Logger.error(s"Trying to progress l3 field declaration for field $name; this is not supported")
  def addToKnowledge() : Unit
}
