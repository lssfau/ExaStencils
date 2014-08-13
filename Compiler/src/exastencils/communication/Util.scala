package exastencils.communication

import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

case class GeneratedMPITag(var from : Expression, var to : Expression, var concurrencyId : Int) extends Expression with Expandable {
  override def cpp(out : CppStream) : Unit = out << "NOT VALID ; CLASS = RemoteSend\n"

  def expand : Output[Expression] = {
    ("((unsigned int)" ~ from ~ " << 20)") + ("((unsigned int)(" ~ to ~ ") << 10)") + concurrencyId
  }
}
