package exastencils.baseExt.ir

import exastencils.base.ir._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.prettyprinting.PpStream

trait IR_MultiDimFieldAccess extends IR_Expression {
  def fieldSelection : FieldSelection
  def index : IR_ExpressionIndex // TODO: IR_Index, also in subclasses
}

/// IR_DirectFieldAccess

case class IR_DirectFieldAccess(var fieldSelection : FieldSelection, var index : IR_ExpressionIndex) extends IR_MultiDimFieldAccess {
  override def datatype = fieldSelection.fieldLayout.datatype
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = DirectFieldAccess\n"

  def linearize = IR_LinearizedFieldAccess(fieldSelection, Mapping.resolveMultiIdx(fieldSelection.fieldLayout, index))
}

/// IR_FieldAccess

case class IR_FieldAccess(var fieldSelection : FieldSelection, var index : IR_ExpressionIndex) extends IR_MultiDimFieldAccess {
  override def datatype = fieldSelection.fieldLayout.datatype
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = FieldAccess\n"

  def expandSpecial = IR_DirectFieldAccess(fieldSelection, index + fieldSelection.referenceOffset)
}

/// IR_LinearizedFieldAccess

case class IR_LinearizedFieldAccess(var fieldSelection : FieldSelection, var index : IR_Expression) extends IR_Expression with IR_Expandable {
  override def datatype = fieldSelection.fieldLayout.datatype
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LinearizedFieldAccess\n"

  override def expand() = {
    ArrayAccess(
      iv.FieldData(fieldSelection.field, fieldSelection.level, fieldSelection.slot, fieldSelection.fragIdx),
      index,
      Knowledge.data_alignFieldPointers)
  }
}
