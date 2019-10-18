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

package exastencils.interfacing.ir

import exastencils.field.ir._
import exastencils.knowledge.ir.IR_KnowledgeObject

/// IR_ExternalField

case class IR_ExternalField(
    var name : String, // will be used to find the field
    var targetField : IR_Field, // the (internal) field to be copied to/ from
    var fieldLayout : IR_FieldLayout, // represents the number of data points and their distribution in each dimension
    var level : Int // the (geometric) level the field lives on
) extends IR_KnowledgeObject {

  override def createDuplicate() = IR_ExternalField(name, targetField, fieldLayout, level)

  // shortcuts to layout options
  def gridDatatype = fieldLayout.datatype
  def resolveBaseDatatype = fieldLayout.datatype.resolveBaseDatatype
  def resolveDeclType = fieldLayout.datatype.resolveDeclType
  def referenceOffset = fieldLayout.referenceOffset
  def communicatesDuplicated = fieldLayout.communicatesDuplicated
  def communicatesGhosts = fieldLayout.communicatesGhosts
}
