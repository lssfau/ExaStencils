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

package exastencils.field.ir

import exastencils.baseExt.ir.IR_MatShape
import exastencils.boundary.ir.IR_BoundaryCondition
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_Domain
import exastencils.knowledge.ir.IR_LeveledKnowledgeObject

trait IR_FieldLike {
  def name : String // will be used to find the field
  def level : Int // the (geometric) level the field lives on
  def index : Int // (consecutive) index of the field, can be used as array subscript
  def domain : IR_Domain // the (sub)domain the field lives on
  def codeName : String // will be used in the generated source code
  def layout : IR_FieldLayoutLike // represents the number of data points and their distribution in each dimension
  def numSlots : Int // the number of copies of the field to be available; can be used to represent different vector components or different versions of the same field (e.g. Jacobi smoothers, time-stepping)
  def boundary : IR_BoundaryCondition // the boundary condition to be enforced when calling apply bc

  def numDimsGrid = domain.numDims

  // shortcuts to layout options
  def gridDatatype = layout.datatype
  def resolveBaseDatatype = layout.datatype.resolveBaseDatatype
  def resolveDeclType = layout.datatype.resolveDeclType
  def localization = layout.localization
  def referenceOffset = layout.referenceOffset
  def communicatesDuplicated = layout.communicatesDuplicated
  def communicatesGhosts = layout.communicatesGhosts
}

/// IR_Field

case class IR_Field(
    var name : String,
    var level : Int,
    var index : Int,
    var domain : IR_Domain,
    var codeName : String,
    var layout : IR_FieldLayout,
    var numSlots : Int,
    var boundary : IR_BoundaryCondition,
    var matShape: Option[IR_MatShape]
) extends IR_LeveledKnowledgeObject with IR_FieldLike {

  override def createDuplicate() : IR_Field = {
    IR_Field.tupled(Duplicate(IR_Field.unapply(this).get))
  }
}
