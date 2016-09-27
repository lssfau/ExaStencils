package exastencils.hack.ir

import exastencils.base.ir._
import exastencils.prettyprinting.PpStream

/// IR_Native

case class HACK_IR_Native(nativeCode : String) extends IR_Expression {
  override def datatype = IR_UnknownDatatype
  override def prettyprint(out : PpStream) = out << nativeCode
}