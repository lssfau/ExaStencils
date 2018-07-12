package exastencils.util.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.prettyprinting.PpStream

/// IR_Read

object IR_Read {
  def apply(stream : IR_VariableAccess, toRead : IR_Expression*) = new IR_Read(stream, toRead.to[ListBuffer])
}

case class IR_Read(var stream : IR_VariableAccess, var toRead : ListBuffer[IR_Expression]) extends IR_Statement {
  override def prettyprint(out : PpStream) = out << stream << " >> " <<< (toRead, " >> ") << ';'
}
