package exastencils.base.l2

import exastencils.prettyprinting.PpStream

/// L2_FunctionAccess

trait L2_FunctionAccess extends L2_Access {
  def name : String
  def datatype : L2_Datatype
}

/// L2_PlainFunctionAccess

trait L2_PlainFunctionAccess extends L2_FunctionAccess {
  override def prettyprint(out : PpStream) = out << name
}

/// L2_LeveledFunctionAccess

trait L2_LeveledFunctionAccess extends L2_FunctionAccess {
  def level : Int
  override def prettyprint(out : PpStream) = out << name << '@' << level
}
