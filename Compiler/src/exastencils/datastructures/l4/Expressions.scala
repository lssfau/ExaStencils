package exastencils.datastructures.l4

import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.datastructures.ir.ImplicitConversions._

trait Expression extends Node
trait Number extends Expression with ProgressableToIr {
  def value : AnyVal
}

case class StringConstant(value : String) extends Expression

case class IntegerConstant(v : Long) extends Number {
  override def value = v

  def progressToIr : ir.IntegerConstant = ir.IntegerConstant(v)
}

case class FloatConstant(v : Double) extends Number {
  override def value = v

  def progressToIr : ir.FloatConstant = ir.FloatConstant(v)
}

case class BooleanConstant(value : Boolean) extends Expression with ProgressableToIr {
  def progressToIr : ir.BooleanConstant = ir.BooleanConstant(value)
}

case class Identifier(name : String, level : Option[LevelSpecification]) extends Expression with ProgressableToIr {
  def progressToIr : String = {
    if (level.isEmpty)
      name
    else
      name + "_" + level.get
  }
}

case class Variable(identifier : Identifier, datatype : Datatype) extends Expression with ProgressableToIr {
  def progressToIr : ir.VariableAccess = {
    ir.VariableAccess(identifier.progressToIr, Some(datatype.progressToIr))
  }
}

case class BinaryExpression(operator : String, var left : Expression, var right : Expression) extends Expression

case class BooleanExpression(operator : String, var left : Expression, var right : Expression) extends Expression

case class FunctionCallExpression(identifier : Identifier, var arguments : List[Expression]) extends Expression
