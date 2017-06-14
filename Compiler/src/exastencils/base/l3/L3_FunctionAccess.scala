package exastencils.base.l3

import exastencils.base.l4._
import exastencils.prettyprinting.PpStream

/// L3_FunctionAccess

trait L3_FunctionAccess extends L3_Access {
  def name : String
  def datatype : L3_Datatype
  override def progress : L4_FunctionAccess
}

/// L3_PlainFunctionAccess

trait L3_PlainFunctionAccess extends L3_FunctionAccess {
  override def prettyprint(out : PpStream) = out << name
}

/// L3_LeveledFunctionAccess

trait L3_LeveledFunctionAccess extends L3_FunctionAccess {
  def level : Int
  override def prettyprint(out : PpStream) = out << name << '@' << level
}
