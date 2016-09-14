package exastencils.base.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.prettyprinting.PpStream

/// L4_WhileLoop

object L4_WhileLoop {
  def apply(comparison : L4_Expression, body : L4_Statement*) = new L4_WhileLoop(comparison, body.to[ListBuffer])
}

case class L4_WhileLoop(var comparison : L4_Expression, var body : ListBuffer[L4_Statement]) extends L4_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "repeat while " << comparison << "{\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }

  override def progress : IR_WhileLoop = IR_WhileLoop(comparison.progress, body.map(_.progress))
}

/// L4_UntilLoop

object L4_UntilLoop {
  def apply(comparison : L4_Expression, body : L4_Statement*) = new L4_UntilLoop(comparison, body.to[ListBuffer])
}

case class L4_UntilLoop(var comparison : L4_Expression, var body : ListBuffer[L4_Statement]) extends L4_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "repeat until " << comparison << "{\n"
    out <<< body << '\n'
    out << "}\n"
  }

  // TODO: internally process L4_UntilLoops to L4_WhileLoops and remove progress
  override def progress : IR_WhileLoop = IR_WhileLoop(IR_NegationExpression(comparison.progress), body.map(_.progress))
}
