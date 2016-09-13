package exastencils.base.ir

import scala.collection.mutable.ListBuffer

import exastencils.prettyprinting.PpStream

object IR_Scope {
  def apply(body : IR_Statement*) = new IR_Scope(body.to[ListBuffer])
  def apply(body : List[IR_Statement]) = new IR_Scope(body.to[ListBuffer])
}

case class IR_Scope(var body : ListBuffer[IR_Statement]) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "{\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }
}
