package harald.Discrete
import scala.collection.mutable.ListBuffer
import harald.dsl._

class DiscrExpression() {

  def value() : Array[Array[Array[Double]]] = Array()
  def ToStringClass = "DiscrExpression"  
  def getvar : ListBuffer[String] = ListBuffer()
  def printtoDSL4(lev : Int) : String = ""
}


case class DiscrBinOp(operator: String, left: DiscrExpression, right: DiscrExpression) extends DiscrExpression {
  override def toString =  s"${left} ${operator} ${right}"  
  override def ToStringClass = left.ToStringClass + s"${operator}" + right.ToStringClass  
  override def getvar : ListBuffer[String] = left.getvar ++ right.getvar
  override def printtoDSL4(lev : Int) : String = { 
    operator match { 
      case "=" => s"${left.printtoDSL4(lev)} ${operator} ${right.printtoDSL4(lev)}" 
      case "~" => s"${left.printtoDSL4(lev)} \n ${right.printtoDSL4(lev)}"
      case "^" => s"repeat up ${right.printtoDSL4(lev)} \n (${left.printtoDSL4(lev)})"
      case _ => s"(${left.printtoDSL4(lev)} ${operator} ${right.printtoDSL4(lev)})" }
  }
 
  override def value() : Array[Array[Array[Double]]] = {
    var result : Array[Array[Array[Double]]] = Array()
    left match {
      case DiscrStencilLiteral(l,st1) => right match {
        case DiscrStencilLiteral(l,st2) => {
          result = Array.ofDim[Double](l,l,l)
          for (i <- 0 to l-1) 
       for ( j <- 0 to l-1) 
       for ( k <- 0 to l-1) {
         operator match {
           case "+" => result(i)(j)(k) = st1(i)(j)(k) + st2(i)(j)(k)
           case "-" => result(i)(j)(k) = st1(i)(j)(k) + st2(i)(j)(k)
           case _ => println("wrong operator in stencils add")
         }    
       }
        }
      }
      case _ => println("no stencils in discrete op")
    }
    /*
       for (i <- 0 to result.size-1) {
       for ( j <- 0 to result(0).size-1) { 
       for ( k <- 0 to result(0)(0).size-1)
         print(" " + result(i)(j)(k))
         print("\n")
       }
         print("\n")
       }
      */ 
       
    return result
  }
}

case class DiscrVariable(obj: DiscrObject, entry : Int = -1) extends DiscrExpression {
  override def ToStringClass = "DiscrVariable " + obj.name   
  override def getvar : ListBuffer[String] = ListBuffer(obj.name)
  override def toString = { 
    var entrystr = ""
    if (entry != -1)
      entrystr = s"<${entry}>"
     s"${obj.name}${entrystr}"
  }
  override def printtoDSL4(lev : Int) : String = s"${obj.name}"
}

case class DiscrUnaryOp(operator: String, expr: DiscrExpression) extends DiscrExpression {
  override def toString =  s"${operator} ${expr}"    
  override def ToStringClass = s"DiscrUnaryOp ${operator} " + expr.ToStringClass 
  override def getvar : ListBuffer[String] = expr.getvar
  override def printtoDSL4(lev : Int) : String = s"${operator} ${expr.printtoDSL4(lev)}"
}

case class DiscrTranspose(expr: DiscrExpression) extends DiscrExpression {
  override def toString =  s"(${expr})^T"  
  override def ToStringClass = s"DiscrTranspose "  
  override def getvar : ListBuffer[String] = expr.getvar
  override def printtoDSL4(lev : Int) : String = s"transpose(${expr.printtoDSL4(lev)})"
}

case class DiscrInverse(expr: DiscrExpression) extends DiscrExpression {
  override def toString =  s"(${expr})^{-1}"  
  override def ToStringClass = s"DiscrInverse "  
  override def getvar : ListBuffer[String] = expr.getvar
  override def printtoDSL4(lev : Int) : String = s"inverse(${expr.printtoDSL4(lev)})"
}

case class DiscrDiag(expr: DiscrExpression) extends DiscrExpression {
  override def toString =  s"diag(${expr})"  
  override def ToStringClass = s"DiscrDiag "  
  override def getvar : ListBuffer[String] = expr.getvar
  override def printtoDSL4(lev : Int) : String = s"diag(${expr.printtoDSL4(lev)})"
}


case class DiscrStencilLiteral(len : Int, st : Array[Array[Array[Double]]]) extends DiscrExpression {
  override def ToStringClass = "DiscrStencilLiteral " + len + st   
  override def toString : String =  {
    var str : String = ""
    for (i <- 0 to len-1) {
       for ( j <- 0 to len-1) {
       for ( k <- 0 to len-1) {
          str += " " + st(i)(j)(k)
       }
    str += "\n"
    }
    str += "\n"
    }
    return str
  }
}


// level 3

class DiscrEquation(lhs : DiscrExpression, rhs : DiscrExpression) {
  override def toString = lhs + "=" + rhs
  def getvar : ListBuffer[String] = lhs.getvar ++ rhs.getvar
   
}

case class DiscrLiteral(name : String, lev : String) extends DiscrExpression {
  override def toString = name + "["+lev+"]"
  override def getvar : ListBuffer[String] = ListBuffer(printtoDSL4(0))  
  override def printtoDSL4(l : Int) : String = {
    var ostr = "["
    var estr = "]"
    for (it <- DomainKnowledge.discr_iterations)
      if (it.name.equals(name)) {
        ostr = "_"
        estr = "()"
      }
        
    lev match { 
      case "" => name 
      case "lc" => name + s"${ostr}${l+1}${estr}"
      case "lf" => name + s"${ostr}${l-1}${estr}"
      case _ => name + s"${ostr}${l}${estr}" }
  }
}
/*
case class DiscrScalar(name : String) extends DiscrExpression {
  override def toString = name
  
}

case class DiscrMatrix(name : String) extends DiscrExpression {
  override def toString = name
  
}

case class DiscrVector(name : String) extends DiscrExpression {
  override def toString = name
  
}
*/