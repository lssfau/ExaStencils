package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.Output

object IR_LoopOverLevels {
  def apply(body : IR_Statement*) = new IR_LoopOverLevels(body.to[ListBuffer])

  def defIt = "levelIdx"
}

case class IR_LoopOverLevels(var body : ListBuffer[IR_Statement]) extends IR_ScopedStatement with IR_Expandable {

  import IR_LoopOverLevels._

  override def expand() : Output[IR_ForLoop] = {
    IR_ForLoop(
      IR_VariableDeclaration(IR_IntegerDatatype, defIt, Knowledge.minLevel),
      IR_Lower(defIt, Knowledge.maxLevel + 1),
      IR_PreIncrement(defIt),
      body)
  }
}
