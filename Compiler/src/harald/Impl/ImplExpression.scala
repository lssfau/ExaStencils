package harald.Impl

import scala.collection.mutable.ListBuffer
import harald.dsl._
import harald.ast.TreeManager

  class ImplExpression() extends ImplBase {
    override def toString = "base E"
    var value: String = ""
    def evaluate(para: ListBuffer[ParameterInfo]): Int = 0
  }

  class ImplValueExpr[T](v: T) extends ImplExpression() {
    value = v.toString
    override def toString = v.toString
    override def toString_cpp: String = v.toString
    override def toString_cuda: String = v.toString
    override def costs(para: String) : Map[Int,String] = CostInfo.get(para).getOrElse(Map())
    override def evaluate(para: ListBuffer[ParameterInfo]): Int = {
      return v.toString.toInt
    }
  }

  class ImplBinaryExpr(left: ImplExpression, oper: OperatorInfo, right: ImplExpression) extends ImplExpression() {
    val l = left
    val op = oper
    val r = right
    value = l.toString + " " + op.toString + " " + r.toString

    override def toString = "(" + l.toString + " " + op.toString + " " + r.toString + ")"
    override def toString_cpp: String = "(" + l.toString_cpp + " " + op.toString_cpp + " " + r.toString_cpp + ")"
    override def toString_cuda: String = "(" + l.toString_cuda + " " + op.toString_cpp + " " + r.toString_cuda + ")"
    override def costs(para: String): Map[Int,String] = {

      var hm : Map[Int,String] = Map()
      hm += 1 -> ""
      CostInfo += op.toString -> hm
      var m : Map[Int,String] = Map()
      m ++= l.costs(para)
      m ++= r.costs(para)
      m ++=  CostInfo.get(para).getOrElse(Map())
      return m
    }
    override def evaluate(para: ListBuffer[ParameterInfo]): Int = {
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
        case _ => return 0
      }
    }
  }

    class ImplVariable(obj: String, n: String, vartype: TypeInfo, lev: ImplExpression, ScopeInfo: String) extends ImplExpression() {
    val name = n
    value = n

    override def toString_cpp: String = {
      var s: String = ""
      if (vartype.d == 1)
        if (ScopeInfo.equals("statement"))
          s = vartype.toString_cpp
        else if (ScopeInfo.equals("expression"))
          s = vartype.toString_cpp
      var objs = obj
      if (!objs.equals(""))
        objs = objs + "."

      if (lev.value.equals(""))
        return objs + name + lev.toString_cpp + s
      else
        return objs + name + "[" + lev.toString_cpp + "]" + s
    }

    override def toString_cuda: String = {
      var s: String = ""
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
    
    override def costs(para: String): Map[Int,String] = {
      //CostInfo += n -> "1"
      //println(CostInfo)
      if (vartype.d == 0)
        return Map()
      else if (vartype.d == 1) {
        if (ScopeInfo.equals("statement"))
          CostInfo += "Store" -> Map(1 -> s"${name}") //s"Store${name}"
        else if (ScopeInfo.equals("expression"))
          CostInfo += "Load" -> Map(1 -> s"${name}")

        return CostInfo.get(para).getOrElse(Map())
      } else
        return Map()
    }
    override def evaluate(para: ListBuffer[ParameterInfo]): Int = {

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

      class ImplFcall(obj: String, name: String, paramlist: ListBuffer[ImplExpression]) extends ImplExpression {
    override def toString_cpp: String = {
      var objs = obj
      if (!objs.equals(""))
        objs = objs + "."

      var s: String = objs + name + " ( "
      if (paramlist.length > 0)
        s = s + paramlist(0).toString_cpp
      for (i <- 1 to paramlist.length - 1)
        s = s + "," + paramlist(i).toString_cpp
      return s + ")\n"

    }
    
    override def toString_cuda: String = {
      var objs = obj
      if (!objs.equals(""))
        objs = objs + "."

      var s: String = objs + name + " ( "
      if (paramlist.length > 0)
        s = s + paramlist(0).toString_cpp
      for (i <- 1 to paramlist.length - 1)
        s = s + "," + paramlist(i).toString_cpp
      return s + ")\n"

    }

    override def costs(para: String): Map[Int,String] = {
      var objs = obj
      if (!objs.equals("")) {
        for (c1 <- TreeManager.tree.ExternalClasses)
          for (c2 <- c1._2.memberfunc)
            if (c2.name.equals(name))
              return c2.costs(para)
      }

      return Map()
      //      DomainKnowledge.CostInfo += "call" -> (DomainKnowledge.CostInfo.getOrElse("call", 0) + 1) 
    }
  }
