package exastencils.interfacing.ir

import exastencils.field.ir._
import exastencils.knowledge.ir.IR_KnowledgeObjectWithIdent

/// IR_ExternalField

object IR_ExternalField {
  exastencils.core.Duplicate.dontCloneHierarchy(this.getClass)
}

case class IR_ExternalField(
    var identifier : String, // will be used to find the field
    var targetField : IR_Field, // the (internal) field to be copied to/ from
    var fieldLayout : IR_FieldLayout, // represents the number of data points and their distribution in each dimension
    var level : Int // the (geometric) level the field lives on
) extends IR_KnowledgeObjectWithIdent {
  // shortcuts to layout options
  def gridDatatype = fieldLayout.datatype
  def resolveBaseDatatype = fieldLayout.datatype.resolveBaseDatatype
  def resolveDeclType = fieldLayout.datatype.resolveDeclType
  def referenceOffset = fieldLayout.referenceOffset
  def communicatesDuplicated = fieldLayout.communicatesDuplicated
  def communicatesGhosts = fieldLayout.communicatesGhosts
}
