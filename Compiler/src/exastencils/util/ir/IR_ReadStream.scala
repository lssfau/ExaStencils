package exastencils.util.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_Statement
import exastencils.prettyprinting.PpStream

case class IR_ReadStream(var stream : IR_Expression, var toPrint : ListBuffer[IR_Expression]) extends IR_Statement {
  override def prettyprint(out : PpStream) = out << stream << " >> " <<< (toPrint, " >> ") << ';'
}
