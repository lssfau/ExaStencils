package exastencils.field.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.datastructures._
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.logger.Logger

/// IR_MultiDimFieldAccess

trait IR_MultiDimFieldAccess extends IR_Expression with IR_SpecialExpandable {
  def fieldSelection : IR_FieldSelection
  def index : IR_ExpressionIndex // TODO: IR_Index, also in subclasses

  var allowLinearization : Boolean = true
}

/// IR_DirectFieldAccess

case class IR_DirectFieldAccess(
    var fieldSelection : IR_FieldSelection,
    var index : IR_ExpressionIndex) extends IR_MultiDimFieldAccess {

  override def datatype = fieldSelection.fieldLayout.datatype
  def linearize = IR_LinearizedFieldAccess(fieldSelection, fieldSelection.fieldLayout.linearizeIndex(index))
}

/// IR_LinearizeDirectFieldAccess

object IR_LinearizeDirectFieldAccess extends DefaultStrategy("Linearize DirectFieldAccess nodes") {
  this += new Transformation("Linearize", {
    case access : IR_DirectFieldAccess if access.allowLinearization => access.linearize
  })
}

/// IR_FieldAccess

case class IR_FieldAccess(
    var fieldSelection : IR_FieldSelection,
    var index : IR_ExpressionIndex,
    var offset : Option[IR_ExpressionIndex] = None) extends IR_MultiDimFieldAccess {

  override def datatype = fieldSelection.fieldLayout.datatype
  def expandSpecial = {
    if (offset.isDefined) Logger.warn(s"IR_FieldAccess with unresolved offset ${ offset.get.prettyprint() } found")
    IR_DirectFieldAccess(fieldSelection, index + fieldSelection.referenceOffset)
  }
}

/// IR_ResolveFieldAccess

object IR_ResolveFieldAccess extends DefaultStrategy("Resolve FieldAccess nodes") {
  this += new Transformation("Resolve", {
    case access : IR_FieldAccess => access.expandSpecial
  })
}

/// IR_ResolveFieldAccess

object IR_ApplyOffsetToFieldAccess extends DefaultStrategy("Apply offsets to FieldAccess nodes") {
  this += new Transformation("Resolve", {
    case access : IR_FieldAccess if access.offset.isDefined =>
      access.index += access.offset.get
      access.offset = None
      access
  })
}

/// IR_LinearizedFieldAccess

case class IR_LinearizedFieldAccess(var fieldSelection : IR_FieldSelection, var index : IR_Expression) extends IR_Expression with IR_Expandable {
  override def datatype = fieldSelection.fieldLayout.datatype

  override def expand() = {
    IR_ArrayAccess(
      IR_IV_FieldData(fieldSelection.field, fieldSelection.level, fieldSelection.slot, fieldSelection.fragIdx),
      index,
      Knowledge.data_alignFieldPointers)
  }
}
