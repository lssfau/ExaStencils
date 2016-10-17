package exastencils.base.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Scope
import exastencils.prettyprinting.PpStream

object L4_Scope {
  def apply(body : L4_Statement*) = new L4_Scope(body.to[ListBuffer])
  def apply(body : List[L4_Statement]) = new L4_Scope(body.to[ListBuffer])
}

case class L4_Scope(var body : ListBuffer[L4_Statement]) extends L4_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "{\n"
    out <<< (body, "\n") << '\n'
    out << '}'
  }

  override def progress : IR_Scope = IR_Scope(body.map(_.progress))
}
