package exastencils.deprecated.harald.Continuous

import exastencils.deprecated.harald.Discrete
import exastencils.deprecated.harald.Discrete._
import exastencils.deprecated.harald.dsl._

case class ContEquation(lhs : ContExpression, rhs : ContExpression, domainpart : String, typeofeq : String) {
  override def toString = s"${ typeofeq } : ${ lhs } = ${ rhs } in ${ domainpart }"
  def ToStringClass = "ContEquation"
}

sealed abstract class ContExpression() {

  def discretize(distype : String, order : Int) : DiscrExpression = new Discrete.DiscrExpression()
  def ToStringClass = "ContExpression"
}

case class ContBinOp(operator : String, left : ContExpression, right : ContExpression) extends ContExpression {
  override def toString = s"${ left } ${ operator } ${ right }"

  override def discretize(distype : String, order : Int) : DiscrExpression = DiscrBinOp(operator, left.discretize(distype, order), right.discretize(distype, order))
  override def ToStringClass = left.ToStringClass + s"ContBinOp ${ operator }" + right.ToStringClass
}

case class ContUnaryOp(operator : String, expr : ContExpression) extends ContExpression {
  override def toString = s"${ operator } ${ expr }"
  override def ToStringClass = s"ContUnaryOp ${ operator } " + expr.ToStringClass
}

case class ContOpapply(op : ContOperator, fct : ContFunction) extends ContExpression {
  override def toString = s"${ op.name }(${ fct.name })"
  override def ToStringClass = s"ContOpapply "
}

case class ContTranspose(expr : ContExpression) extends ContExpression {
  override def toString = s"(${ expr })^T"
  override def ToStringClass = s"ContTranspose "
}

case class ContFuncapply(fctname : String, expr : ContExpression) extends ContExpression {
  override def toString = s"${ fctname }(${ expr })"
  override def ToStringClass = s"ContFuncapply " + expr.ToStringClass
}

case class ContIntegral(domainpart : String, expr : ContExpression) extends ContExpression {
  override def toString = s"integral_${ domainpart } (${ expr })"
  override def ToStringClass = s"ContIntegral " + expr.ToStringClass
}

case class ContLiteral(value : String) extends ContExpression {
  override def toString = value
  override def ToStringClass = s"ContLiteral " + value

  override def discretize(distype : String, order : Int) : DiscrExpression = {

    val h = 1 //4096
    var x : Array[Double] = Array()
    if (order == 2)
      x = Array(-1.0 / h, 0, 1.0 / h)
    else if (order == 4)
      x = Array(-2.0 / h, -1.0 / h, 0, 1.0 / h, 2.0 / h)

    val erg = DomainKnowledge.fornberg(0, x, 4)
    /*     for (i <- 0 to erg.length-1) {
             for ( j <- 0 to erg(i).length-1) {
                print(" " + erg(i)(j));
             }
             println();
          }
     */
    val x2 : Array[Double] = Array.ofDim(erg(0).length - 1)
    val xh : Array[Double] = Array.ofDim(erg(0).length - 1)
    val xerg : Array[Array[Array[Double]]] = Array.ofDim[Double](x2.length, x2.length, x2.length)

    value match {
      case "dx"  => {

        val no = 2
        for (i <- 1 to erg(no).length - 1)
          x2(i - 1) = erg(no)(i)
        xh((erg(no).length - 1) / 2) = 1
        for (i <- 0 to x2.length - 1) {
          for (j <- 0 to x2.length - 1) {
            for (k <- 0 to x2.length - 1) {
              xerg(i)(j)(k) += x2(k) * xh(j) * xh(i)
            }
          }
        }
        new Discrete.DiscrStencilLiteral(x2.length, xerg)
      }
      case "dx2" => {

        val no = 3
        for (i <- 1 to erg(no).length - 1)
          x2(i - 1) = erg(no)(i)
        xh((erg(no).length - 1) / 2) = 1
        for (i <- 0 to x2.length - 1) {
          for (j <- 0 to x2.length - 1) {
            for (k <- 0 to x2.length - 1) {
              xerg(i)(j)(k) += x2(k) * xh(j) * xh(i)
              //        print(" " + xerg(i)(j)(k));
            }
            //   println()
          }
          //   println()
          //   println()
        }
        new Discrete.DiscrStencilLiteral(x2.length, xerg)
      }
      case "dy2" => {
        val no = 3
        for (i <- 1 to erg(no).length - 1)
          x2(i - 1) = erg(no)(i)
        xh((erg(no).length - 1) / 2) = 1
        for (i <- 0 to x2.length - 1) {
          for (j <- 0 to x2.length - 1) {
            for (k <- 0 to x2.length - 1) {
              xerg(i)(j)(k) += x2(j) * xh(i) * xh(k)
            }
          }
        }
        new Discrete.DiscrStencilLiteral(x2.length, xerg)
      }
      case "dz2" => {
        val no = 3
        for (i <- 1 to erg(no).length - 1)
          x2(i - 1) = erg(no)(i)
        xh((erg(no).length - 1) / 2) = 1
        for (i <- 0 to x2.length - 1) {
          for (j <- 0 to x2.length - 1) {
            for (k <- 0 to x2.length - 1) {
              xerg(i)(j)(k) += x2(i) * xh(j) * xh(k)
            }
          }
        }
        new Discrete.DiscrStencilLiteral(x2.length, xerg)
      }
      case _     => new Discrete.DiscrExpression()
    }
  }
}

/*
case class ContVecExpression(exprlist : List[ContExpression], transposed : Boolean = false) extends ContExpression {
  override def toString : String = {
    var str = s"(${exprlist(0)}"
    for (i <- 1 to exprlist.length-1) {
      str += s",${exprlist(i)}"
    }
    str += ")"
    if (transposed)
      str += "^T"
    return str   
  }
}

case class ContVecLiteral(values: List[String]) extends ContExpression {
  override def toString : String = {
    var str = s"(${values(0)}"
    for (i <- 1 to values.length-1) {
      str += s",${values(i)}"
    }
    str += ")"
    return str   
  }
}
*/
case class ContVariable(obj : ContObject, entry : Int = -1) extends ContExpression {
  override def ToStringClass = s"ContVariable " + obj.toString() + " " + entry
  override def toString = {
    var entrystr = ""
    if (entry != -1)
      entrystr = s"<${ entry }>"
    s"${ obj.name }${ entrystr }"
  }

  override def discretize(distype : String, order : Int) : DiscrExpression = {
    obj match {
      case ContConstant(a, b, c) =>
        for (o <- DomainKnowledge.discr_consts)
          if (o.name.equals(a))
            return DiscrVariable(o, entry)
      case ContFunction(a, b)    =>
        for (o <- DomainKnowledge.discr_functions)
          if (o.name.equals(a))
            return DiscrVariable(o, entry)
      case ContOperator(a, b)    =>
        for (o <- DomainKnowledge.cont_equations)
          o.lhs match {
            case ContLiteral(str) => println("matches" + str); return o.rhs.discretize(distype, order)
            case _                =>
          }
    }
    println("try to")
    return new DiscrExpression
  }
}
