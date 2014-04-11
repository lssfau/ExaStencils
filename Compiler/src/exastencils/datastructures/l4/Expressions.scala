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

abstract class Identifier(var name : String) extends Expression with ProgressableToIr

case class UnresolvedIdentifier(var name2 : String, var level : Option[LevelSpecification]) extends Identifier(name2) {
  def progressToIr : String = "ERROR - UnresolvedIdentifier"
}

case class BasicIdentifier(var name2 : String) extends Identifier(name2) {
  def progressToIr : String = name
}

case class LeveledIdentifier(var name2 : String, var level : LevelSpecification) extends Identifier(name2) {
  def progressToIr : String = {
    name + "_" + level.asInstanceOf[SingleLevelSpecification].level
  }
}

case class FieldIdentifier(var name2 : String, var level : LevelSpecification) extends Identifier(name2) {
  def progressToIr : ir.UnresolvedFieldAccess = {
    ir.UnresolvedFieldAccess("curFragment." /*FIXME*/ , name, level.asInstanceOf[SingleLevelSpecification].level, "0" /*FIXME*/ , ir.DefaultLoopMultiIndex())
  }
}

case class StencilIdentifier(var name2 : String, var level : LevelSpecification) extends Identifier(name2) {
  def progressToIr : ir.UnresolvedStencilAccess = {
    ir.UnresolvedStencilAccess(name, level.asInstanceOf[SingleLevelSpecification].level)
  }
}

case class Variable(var identifier : Identifier, var datatype : Datatype) extends Expression with ProgressableToIr {
  def progressToIr : ir.VariableAccess = {
    ir.VariableAccess(identifier.progressToIr.asInstanceOf[String /*FIXME*/ ], Some(datatype.progressToIr))
  }
}

case class BinaryExpression(var operator : String, var left : Expression, var right : Expression) extends Expression with ProgressableToIr {
  def progressToIr : ir.BinaryExpression = {
    ir.BinaryExpression(operator, left.asInstanceOf[ProgressableToIr].progressToIr.asInstanceOf[ir.Expression], right.asInstanceOf[ProgressableToIr].progressToIr.asInstanceOf[ir.Expression])
  }
}

case class BooleanExpression(operator : String, var left : Expression, var right : Expression) extends Expression

case class FunctionCallExpression(identifier : Identifier, var arguments : List[Expression]) extends Expression
