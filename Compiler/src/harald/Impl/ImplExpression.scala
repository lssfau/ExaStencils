package harald.Impl

import scala.collection.mutable.ListBuffer
import harald.dsl._
import harald.ast.TreeManager
import exastencils.datastructures.ir._

case class ImplVariable(obj : String, n : String, vartype : TypeInfo, lev : Expression, ScopeInfo : String) extends Expression() {
  override def duplicate = this.copy().asInstanceOf[this.type]

  val name = n
  //valu = n

  override def cpp : String = {
    var s : String = ""
    if (vartype.d == 1)
      if (ScopeInfo.equals("statement"))
        s = vartype.toString_cpp
      else if (ScopeInfo.equals("expression"))
        s = vartype.toString_cpp
    var objs = obj
    if (!objs.equals(""))
      objs = objs + "."

    if (lev.cpp /*FIXME: this mechanism is really hard to maintain*/ .equals(""))
      return objs + name + lev.cpp + s
    else
      return objs + name + "[" + lev.cpp + "]" + s
  }

  def toString_cuda : String = {
    var s : String = ""
    if (vartype.d == 1)
      if (ScopeInfo.equals("statement"))
        s = vartype.toString_cuda
      else if (ScopeInfo.equals("expression"))
        s = vartype.toString_cuda
    var objs = obj
    if (!objs.equals(""))
      objs = objs + "."

    return objs + name + s
  }

  override def evaluate(para : ListBuffer[ParameterInfo]) : Int = {

    // println(para + " " + n)
    for (p <- para)
      if (p.name.equals(n))
        return p.value

    for (c <- DomainKnowledge.global_variables)
      if (c.name.equals(n))
        return c.value

    return 0
  }
}
