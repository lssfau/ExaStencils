package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer
import exastencils.knowledge
import exastencils.knowledge.Knowledge
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.datastructures.ir.ImplicitConversions._

trait Expression extends Node with ProgressableToIr {
  def progressToIr : ir.Expression
}

trait Number extends Expression {
  def value : AnyVal
}

case class StringConstant(var value : String) extends Expression {
  def progressToIr : ir.StringConstant = ir.StringConstant(value)
}

case class IntegerConstant(var v : Long) extends Number {
  override def value = v
  def progressToIr : ir.IntegerConstant = ir.IntegerConstant(v)
}

case class FloatConstant(var v : Double) extends Number {
  override def value = v
  def progressToIr : ir.FloatConstant = ir.FloatConstant(v)
}

case class BooleanConstant(var value : Boolean) extends Expression {
  def progressToIr : ir.BooleanConstant = ir.BooleanConstant(value)
}

abstract class Identifier() extends Expression { var name : String }

case class UnresolvedIdentifier(var name : String, var level : Option[LevelSpecification]) extends Identifier {
  def progressToIr : ir.StringConstant = "ERROR - UnresolvedIdentifier"
}

case class BasicIdentifier(var name : String) extends Identifier {
  def progressToIr : ir.StringConstant = name
}

case class LeveledIdentifier(var name : String, var level : LevelSpecification) extends Identifier {
  def progressToIr : ir.StringConstant = {
    name + "_" + level.asInstanceOf[SingleLevelSpecification].level
  }
}

case class FieldIdentifier(var name : String, var level : LevelSpecification) extends Identifier {
  def progressNameToIr : ir.StringConstant = {
    name + "_" + level.asInstanceOf[SingleLevelSpecification].level
  }

  def progressToIr : ir.UnresolvedFieldAccess = {
    var multiIndex = ir.DefaultLoopMultiIndex()
    multiIndex(Knowledge.dimensionality) = 0
    ir.UnresolvedFieldAccess("curFragment." /*FIXME*/ , name, level.asInstanceOf[SingleLevelSpecification].level, "0" /*FIXME*/ , multiIndex)
  }
}

case class VectorFieldAccess(var field : FieldIdentifier, var index : Int) extends Identifier {
  var name = field.name //compliance with Identifier

  def progressNameToIr : ir.StringConstant = {
    field.progressNameToIr
  }

  def progressToIr : ir.UnresolvedFieldAccess = {
    var access = field.progressToIr
    access.index(Knowledge.dimensionality) = index
    access
  }
}

case class StencilIdentifier(var name : String, var level : LevelSpecification) extends Identifier {
  def progressToIr : ir.UnresolvedStencilAccess = {
    ir.UnresolvedStencilAccess(name, level.asInstanceOf[SingleLevelSpecification].level)
  }
}

case class Variable(var identifier : Identifier, var datatype : Datatype) extends Expression {
  def progressToIr : ir.VariableAccess = {
    ir.VariableAccess(identifier.progressToIr.asInstanceOf[ir.StringConstant].value, Some(datatype.progressToIr))
  }
}

case class BinaryExpression(var operator : String, var left : Expression, var right : Expression) extends Expression {
  def progressToIr : ir.Expression = {
    ir.BinaryOperators.CreateExpression(operator, left.progressToIr, right.progressToIr)
  }
}

case class BooleanExpression(var operator : String, var left : Expression, var right : Expression) extends Expression {
  def progressToIr : ir.Expression = {
    ir.BinaryOperators.CreateExpression(operator, left.progressToIr, right.progressToIr)
  }
}

case class FunctionCallExpression(var identifier : Identifier, var arguments : List[Expression]) extends Expression {
  def progressToIr : ir.FunctionCallExpression = {
    ir.FunctionCallExpression(identifier.progressToIr.asInstanceOf[ir.StringConstant].value,
      arguments.map(s => s.progressToIr).to[ListBuffer])
  }
}
