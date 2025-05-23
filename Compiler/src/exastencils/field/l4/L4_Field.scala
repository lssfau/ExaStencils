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
import exastencils.baseExt.l4.L4_MatShape
import exastencils.boundary.l4.L4_BoundaryCondition
import exastencils.core.Duplicate
import exastencils.domain.l4.L4_Domain
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_FieldLayout
import exastencils.fieldlike.l4.L4_FieldLike
import exastencils.prettyprinting._

/// L4_Field

case class L4_Field(
    var name : String,
    var level : Int,
    var index : Int,
    var domain : L4_Domain,
    var fieldLayout : L4_FieldLayout,
    var numSlots : Int,
    var boundary : L4_BoundaryCondition,
    var matShape : Option[L4_MatShape] = None,
    var gpuCompatible : Boolean = true
) extends L4_FieldLike[IR_Field, IR_FieldLayout] {

  override def createDuplicate() : L4_Field = {
    L4_Field.tupled(Duplicate(L4_Field.unapply(this).get))
  }

  override def prettyprintDecl(out : PpStream) = {
    out << "Field " << name
    out << "< " << domain.name << ", " << fieldLayout.name << ", " << boundary
    if (matShape.isDefined) out << ", " << matShape
    out << " >"
    if (numSlots > 1) out << "[" << numSlots << "]"
    out << "@" << level
  }

  override def progressImpl() = {
    IR_Field(
      name,
      level,
      index,
      domain.getProgressedObj(),
      name.toLowerCase + "Data_" + level,
      fieldLayout.getProgressedObj(),
      numSlots,
      boundary.progress,
      if(matShape.isDefined) Some(matShape.get.progress) else None,
      gpuCompatible
    )
  }

  override def getFieldAccess(slot : L4_SlotSpecification, offset : Option[L4_ConstIndex], frozen : Boolean, matIndex : Option[L4_MatIndex]) : L4_FieldAccess =
    L4_FieldAccess(this, slot, offset,frozen, matIndex)
}
