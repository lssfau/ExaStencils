package exastencils.base.ir

import exastencils.prettyprinting.PpStream

/// IR_FunctionAccess

// TODO: split similar to l4?
case class IR_FunctionAccess(var name : String, var datatype : IR_Datatype) extends IR_Access {
  override def prettyprint(out : PpStream) = out << name
}
