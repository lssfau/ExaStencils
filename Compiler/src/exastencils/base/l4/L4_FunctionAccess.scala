package exastencils.base.l4

import exastencils.base.ir.IR_FunctionAccess
import exastencils.prettyprinting.PpStream

/// L4_FunctionAccess

trait L4_FunctionAccess extends L4_Access {
  def name : String
  def datatype : L4_Datatype
  override def progress : IR_FunctionAccess
}

/// L4_PlainFunctionAccess

trait L4_PlainFunctionAccess extends L4_FunctionAccess {
  override def prettyprint(out : PpStream) = out << name
}

/// L4_LeveledFunctionAccess

trait L4_LeveledFunctionAccess extends L4_FunctionAccess {
  def level : Int
  override def prettyprint(out : PpStream) = out << name << '@' << level
}
