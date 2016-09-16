package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.datastructures.Transformation.Output
import exastencils.knowledge.Knowledge
import exastencils.prettyprinting.PpStream

object IR_LoopOverLevels {
  def apply(body : IR_Statement*) = new IR_LoopOverLevels(body.to[ListBuffer])

  def defIt = "levelIdx"
}

case class IR_LoopOverLevels(var body : ListBuffer[IR_Statement]) extends IR_Statement with IR_Expandable {

  import IR_LoopOverLevels._

  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() : Output[IR_ForLoop] = {
    IR_ForLoop(
      IR_VariableDeclaration(IR_IntegerDatatype, defIt, Knowledge.minLevel),
      IR_LowerExpression(defIt, Knowledge.maxLevel + 1),
      IR_PreIncrementExpression(defIt),
      body)
  }
}
