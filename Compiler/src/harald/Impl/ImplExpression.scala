package harald.Impl

import scala.collection.mutable.ListBuffer
import harald.dsl._
import harald.ast.TreeManager
import exastencils.datastructures.ir._

//  class Expression() extends ImplBase {
//    override def toString = "base E"
//    var value: String = ""
//    def evaluate(para: ListBuffer[ParameterInfo]): Int = 0
//  }

case class ImplValueExpr[T](v : T) extends Expression() {
  override def duplicate = this.copy().asInstanceOf[this.type]

  valu = v.toString
  override def toString = v.toString
  override def cpp : String = v.toString
  def toString_cuda : String = v.toString
  override def evaluate(para : ListBuffer[ParameterInfo]) : Int = {
    return v.toString.toInt
  }
}

case class ImplBinaryExpr(left : Expression, oper : OperatorInfo, right : Expression) extends Expression() {
  override def duplicate = this.copy().asInstanceOf[this.type]

  val l = left
  val op = oper
  val r = right
  valu = l.toString + " " + op.toString + " " + r.toString

  override def toString = "(" + l.toString + " " + op.toString + " " + r.toString + ")"
  override def cpp : String = "(" + l.cpp + " " + op.toString_cpp + " " + r.cpp + ")"
  // FIXME override def toString_cuda: String = "(" + l.toString_cuda + " " + op.cpp + " " + r.toString_cuda + ")"
  override def evaluate(para : ListBuffer[ParameterInfo]) : Int = {
    oper.name match {
      case "==" => {
        if (left.evaluate(para) == right.evaluate(para))
          return 1
        else
          return 0
      }
      case "+" => return left.evaluate(para) + right.evaluate(para)
      case "-" => return left.evaluate(para) - right.evaluate(para)
      case "*" => return left.evaluate(para) * right.evaluate(para)
      case "/" => return left.evaluate(para) / right.evaluate(para)
      case _   => return 0
    }
  }
}

case class ImplVariable(obj : String, n : String, vartype : TypeInfo, lev : Expression, ScopeInfo : String) extends Expression() {
  override def duplicate = this.copy().asInstanceOf[this.type]

  val name = n
  valu = n

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

    if (lev.valu.equals(""))
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

case class ImplFcall(obj : String, name : String, paramlist : ListBuffer[Expression]) extends Expression {
  override def duplicate = this.copy().asInstanceOf[this.type]

  override def cpp : String = {
    var objs = obj
    if (!objs.equals(""))
      objs = objs + "."

    var s : String = objs + name + " ( "
    if (paramlist.length > 0)
      s = s + paramlist(0).cpp
    for (i <- 1 to paramlist.length - 1)
      s = s + "," + paramlist(i).cpp
    return s + ")\n"
  }

  def toString_cuda : String = {
    var objs = obj
    if (!objs.equals(""))
      objs = objs + "."

    var s : String = objs + name + " ( "
    if (paramlist.length > 0)
      s = s + paramlist(0).cpp
    for (i <- 1 to paramlist.length - 1)
      s = s + "," + paramlist(i).cpp
    return s + ")\n"
  }
}
