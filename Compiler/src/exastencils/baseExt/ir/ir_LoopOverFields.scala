package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.datastructures.Transformation.Output
import exastencils.field.ir.IR_FieldCollection
import exastencils.prettyprinting.PpStream

object IR_LoopOverFields {
  def apply(body : IR_Statement*) = new IR_LoopOverFields(body.to[ListBuffer])

  def defIt = "fieldIdx"
}

case class IR_LoopOverFields(var body : ListBuffer[IR_Statement]) extends IR_Statement with IR_Expandable {

  import IR_LoopOverFields._

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() : Output[IR_ForLoop] = {
    IR_ForLoop(
      IR_VariableDeclaration(IR_IntegerDatatype, defIt, 0),
      IR_LowerExpression(defIt, IR_FieldCollection.objects.size),
      IR_PreIncrementExpression(defIt),
      body)
  }
}
