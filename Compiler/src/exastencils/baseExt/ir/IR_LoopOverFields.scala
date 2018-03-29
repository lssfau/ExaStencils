package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.datastructures.Transformation.Output
import exastencils.field.ir.IR_FieldCollection

object IR_LoopOverFields {
  def apply(body : IR_Statement*) = new IR_LoopOverFields(body.to[ListBuffer])

  def defIt = "fieldIdx"
}

case class IR_LoopOverFields(var body : ListBuffer[IR_Statement]) extends IR_ScopedStatement with IR_Expandable {

  import IR_LoopOverFields._

  override def expand() : Output[IR_ForLoop] = {
    IR_ForLoop(
      IR_VariableDeclaration(IR_IntegerDatatype, defIt, 0),
      IR_Lower(defIt, IR_FieldCollection.objects.size),
      IR_PreIncrement(defIt),
      body)
  }
}
