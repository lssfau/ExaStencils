package exastencils.base.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_UnaryOp extends Generatable {
  override def validLayers() = ListBuffer(L2, L3, L4)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/base/|LAYER_LC|/|LAYER_UC|_UnaryOp.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.base.|LAYER_LC|

import exastencils.base.|NEXT_LC|._
import exastencils.prettyprinting._

/// |LAYER_UC|_UnaryOperators

object |LAYER_UC|_UnaryOperators extends Enumeration {
  exastencils.core.Duplicate.registerImmutable(this.getClass)

  type UnaryOperators = Value
  val Negative = Value("-")
  val Not = Value("!")

  def createExpression(op : String, exp : |LAYER_UC|_Expression) : |LAYER_UC|_Expression = createExpression(withName(op), exp)
  def createExpression(op : Value, exp : |LAYER_UC|_Expression) : |LAYER_UC|_Expression = op match {
    case Negative => |LAYER_UC|_Negative(exp)
    case Not      => |LAYER_UC|_Negation(exp)
  }
}

/// arithmetic operations

case class |LAYER_UC|_Negative(var left : |LAYER_UC|_Expression) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << "(-" << left << ')'
  override def progress = |NEXT_UC|_Negative(left.progress)
}

/// logical operations

case class |LAYER_UC|_Negation(var left : |LAYER_UC|_Expression) extends |LAYER_UC|_Expression {
  override def prettyprint(out : PpStream) : Unit = out << '!' << '(' << left << ')'
  override def progress = |NEXT_UC|_Negation(left.progress)
}
"""
  }
}
