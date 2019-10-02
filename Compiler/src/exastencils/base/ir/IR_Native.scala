package exastencils.base.ir

import exastencils.prettyprinting.PpStream

/// IR_Native

case class IR_Native(nativeCode : String) extends IR_Expression {
  override def datatype = IR_UnknownDatatype
  override def prettyprint(out : PpStream) = out << nativeCode
}
