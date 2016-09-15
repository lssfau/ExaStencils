package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir._
import exastencils.knowledge.FieldCollection
import exastencils.prettyprinting.PpStream

object IR_LoopOverFields {
  def apply(body : IR_Statement*) = new IR_LoopOverFields(body.to[ListBuffer])

  def defIt = "fieldIdx"
}

case class IR_LoopOverFields(var body : ListBuffer[IR_Statement]) extends IR_Statement with Expandable {

  import IR_LoopOverFields._

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = LoopOverFields\n"

  override def expand() : Output[IR_ForLoop] = {
    IR_ForLoop(
      VariableDeclarationStatement(IR_IntegerDatatype, defIt, Some(0)),
      IR_LowerExpression(defIt, FieldCollection.fields.size),
      IR_PreIncrementExpression(defIt),
      body)
  }
}
