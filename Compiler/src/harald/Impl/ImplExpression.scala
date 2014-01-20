package harald.Impl

import scala.collection.mutable.ListBuffer
import harald.dsl._
import harald.ast.TreeManager
import exastencils.datastructures.ir._

case class ImplVariable(name : String, vartype : TypeInfo, lev : Expression, ScopeInfo : String) extends Expression() {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = {
    var s : String = ""
    if (vartype.d == 1)
      if (ScopeInfo.equals("statement"))
        s = vartype.toString_cpp
      else if (ScopeInfo.equals("expression"))
        s = vartype.toString_cpp

    if (lev.cpp /*FIXME: this mechanism is really hard to maintain*/ .equals(""))
      return name + lev.cpp + s
    else
      return name + "[" + lev.cpp + "]" + s
  }

  def toString_cuda : String = {
    var s : String = ""
    if (vartype.d == 1)
      if (ScopeInfo.equals("statement"))
        s = vartype.toString_cuda
      else if (ScopeInfo.equals("expression"))
        s = vartype.toString_cuda

    return name + s
  }
}
