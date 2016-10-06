package exastencils.field.ir

import exastencils.base.ir._
import exastencils.config._
import exastencils.datastructures.ir._
import exastencils.deprecated.ir.IR_FieldSelection
import exastencils.prettyprinting.PpStream

trait IR_MultiDimFieldAccess extends IR_Expression {
  def fieldSelection : IR_FieldSelection
  def index : IR_ExpressionIndex // TODO: IR_Index, also in subclasses
}

/// IR_DirectFieldAccess

case class IR_DirectFieldAccess(var fieldSelection : IR_FieldSelection, var index : IR_ExpressionIndex) extends IR_MultiDimFieldAccess {
  override def datatype = fieldSelection.fieldLayout.datatype
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def linearize = IR_LinearizedFieldAccess(fieldSelection, fieldSelection.fieldLayout.linearizeIndex(index))
}

/// IR_FieldAccess

case class IR_FieldAccess(var fieldSelection : IR_FieldSelection, var index : IR_ExpressionIndex) extends IR_MultiDimFieldAccess {
  override def datatype = fieldSelection.fieldLayout.datatype
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def expandSpecial = IR_DirectFieldAccess(fieldSelection, index + fieldSelection.referenceOffset)
}

/// IR_LinearizedFieldAccess

case class IR_LinearizedFieldAccess(var fieldSelection : IR_FieldSelection, var index : IR_Expression) extends IR_Expression with IR_Expandable {
  override def datatype = fieldSelection.fieldLayout.datatype
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() = {
    IR_ArrayAccess(
      iv.FieldData(fieldSelection.field, fieldSelection.level, fieldSelection.slot, fieldSelection.fragIdx),
      index,
      Knowledge.data_alignFieldPointers)
  }
}
