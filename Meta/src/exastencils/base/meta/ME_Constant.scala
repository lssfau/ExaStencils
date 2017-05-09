package exastencils.base.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_Constant extends Generatable {
  override def validLayers() = ListBuffer(L2, L3, L4)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/base/|LAYER_LC|/|LAYER_UC|_Constant.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.base.|LAYER_LC|

import exastencils.base.|NEXT_LC|._
import exastencils.datastructures._
import exastencils.prettyprinting._

/// |LAYER_UC|_ConstantExpression

trait |LAYER_UC|_ConstantExpression extends |LAYER_UC|_Expression

/// |LAYER_UC|_Number
trait |LAYER_UC|_Number extends |LAYER_UC|_ConstantExpression {
  def value : AnyVal
}

/// |LAYER_UC|_StringLiteral

case class |LAYER_UC|_StringLiteral(var value : String) extends |LAYER_UC|_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << value
  override def toString : String = value
  override def progress = |NEXT_UC|_StringLiteral(value)
}

/// |LAYER_UC|_StringConstant

case class |LAYER_UC|_StringConstant(var value : String) extends |LAYER_UC|_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << '"' << value << '"'
  override def progress = |NEXT_UC|_StringConstant(value)
}

/// |LAYER_UC|_IntegerConstant
case class |LAYER_UC|_IntegerConstant(var v : Long) extends |LAYER_UC|_Number {
  override def prettyprint(out : PpStream) : Unit = out << v
  override def value = v
  override def progress = |NEXT_UC|_IntegerConstant(value)
}

/// |LAYER_UC|_RealConstant
case class |LAYER_UC|_RealConstant(var v : Double) extends |LAYER_UC|_Number {
  override def prettyprint(out : PpStream) : Unit = {
    out << value // this uses value.toString(), which is Locale-independent and the string can be parsed without a loss of precision later
  }
  override def value = v
  override def progress = |NEXT_UC|_RealConstant(value)
}

/// |LAYER_UC|_BooleanConstant

case class |LAYER_UC|_BooleanConstant(var value : Boolean) extends |LAYER_UC|_ConstantExpression {
  override def prettyprint(out : PpStream) : Unit = out << value
  override def progress = |NEXT_UC|_BooleanConstant(value)
}

// |LAYER_UC|_ConvertStringConstantsToLiterals

object |LAYER_UC|_ConvertStringConstantsToLiterals extends QuietDefaultStrategy("Convert string constants to literals") {
  this += new Transformation("Convert", {
    case const : |LAYER_UC|_StringConstant => |LAYER_UC|_StringLiteral(const.value)
  })
}

object |LAYER_UC|_ConvertStringLiteralsToConstants extends QuietDefaultStrategy("Convert string literals to constants") {
  this += new Transformation("Convert", {
    case const : |LAYER_UC|_StringLiteral => |LAYER_UC|_StringConstant(const.value)
  })
}
"""
  }
}
