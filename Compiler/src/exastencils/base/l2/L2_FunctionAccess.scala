package exastencils.base.l2

import exastencils.base.l3._
import exastencils.prettyprinting.PpStream

/// L2_FunctionAccess

trait L2_FunctionAccess extends L2_Access {
  // name is read/write
  var name : String
  def datatype : L2_Datatype

  override def prettyprint(out : PpStream) = out << name
}

/// L2_UserFunctionAccess

case class L2_UserFunctionAccess(var name : String, var datatype : L2_Datatype) extends L2_FunctionAccess {
  override def progress = L3_UserFunctionAccess(name, datatype.progress)
}
