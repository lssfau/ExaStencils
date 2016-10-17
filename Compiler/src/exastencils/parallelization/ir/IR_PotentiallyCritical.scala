package exastencils.parallelization.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Statement
import exastencils.prettyprinting.PpStream

/// IR_PotentiallyCritical

object IR_PotentiallyCritical {
  def apply(body : IR_Statement*) = new IR_PotentiallyCritical(body.to[ListBuffer])
  def apply(body : List[IR_Statement]) = new IR_PotentiallyCritical(body.to[ListBuffer])
}

case class IR_PotentiallyCritical(var body : ListBuffer[IR_Statement]) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "{\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}
