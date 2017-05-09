package exastencils.base.l3

import exastencils.base.l4.L4_UserFunctionAccess
import exastencils.prettyprinting.PpStream

/// L3_FunctionAccess

trait L3_FunctionAccess extends L3_Access {
  // name is read/write
  var name : String
  def datatype : L3_Datatype

  override def prettyprint(out : PpStream) = out << name
}

/// L3_UserFunctionAccess

case class L3_UserFunctionAccess(var name : String, var datatype : L3_Datatype) extends L3_FunctionAccess {
  override def progress = L4_UserFunctionAccess(name, datatype.progress)
}
