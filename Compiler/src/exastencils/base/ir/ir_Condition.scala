package exastencils.base.ir

import scala.collection.mutable.ListBuffer

import exastencils.prettyprinting.PpStream

object IR_IfCondition {
  def apply(condition : IR_Expression, trueBody : ListBuffer[IR_Statement]) = new IR_IfCondition(condition, trueBody, ListBuffer())
  def apply(condition : IR_Expression, trueBranch : IR_Statement) = new IR_IfCondition(condition, ListBuffer(trueBranch), ListBuffer())

  def apply(condition : IR_Expression, trueBranch : IR_Statement, falseBranch : IR_Statement) = new IR_IfCondition(condition, ListBuffer(trueBranch), ListBuffer(falseBranch))
  def apply(condition : IR_Expression, trueBody : ListBuffer[IR_Statement], falseBranch : IR_Statement) = new IR_IfCondition(condition, trueBody, ListBuffer(falseBranch))
  def apply(condition : IR_Expression, trueBranch : IR_Statement, falseBody : ListBuffer[IR_Statement]) = new IR_IfCondition(condition, ListBuffer(trueBranch), falseBody)
}

case class IR_IfCondition(var condition : IR_Expression, var trueBody : ListBuffer[IR_Statement], var falseBody : ListBuffer[IR_Statement]) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "if (" << condition << ") {\n"
    out <<< (trueBody, "\n") << '\n'
    if (falseBody.nonEmpty) {
      out << "} else {\n"
      out <<< (falseBody, "\n") << '\n'
    }
    out << '}'
  }
}

