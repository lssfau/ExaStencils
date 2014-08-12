package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils.datastructures._
import exastencils.knowledge

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

abstract class Access() extends Expression {}

case class UnresolvedAccess(var identifier : String, var level : Option[AccessLevelSpecification], var slot : Option[Expression], var arrayIndex : Option[Int]) extends Access {
  def progressToIr : ir.StringConstant = ir.StringConstant("ERROR - Unresolved Access")

  def resolveToBasicOrLeveledAccess = if (level.isDefined) LeveledAccess(identifier, level.get) else BasicAccess(identifier)
  def resolveToFieldAccess = FieldAccess(identifier, level.get, slot.getOrElse(IntegerConstant(0)), arrayIndex.getOrElse(0))
  def resolveToStencilAccess = StencilAccess(identifier, level.get)
  def resolveToStencilFieldAccess = StencilFieldAccess(identifier, level.get, slot.getOrElse(IntegerConstant(0)))
}

case class BasicAccess(var name : String) extends Access {
  def progressToIr : ir.StringConstant = ir.StringConstant(name)
}

case class LeveledAccess(var name : String, var level : AccessLevelSpecification) extends Access {
  def progressToIr : ir.Expression = {
    if ("levels" == name) // TODO: incorporate this into the parser?
      ir.IntegerConstant(level.asInstanceOf[SingleLevelSpecification].level)
    else
      ir.StringConstant(name + "_" + level.asInstanceOf[SingleLevelSpecification].level)
  }
}

case class FieldAccess(var name : String, var level : AccessLevelSpecification, var slot : Expression, var arrayIndex : Int) extends Access {
  def progressNameToIr : ir.StringConstant = {
    ir.StringConstant(name + "_" + level.asInstanceOf[SingleLevelSpecification].level)
  }

  def resolveField : knowledge.Field = {
    knowledge.FieldCollection.getFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get
  }

  def progressToIr : ir.FieldAccess = {
    var multiIndex = ir.LoopOverDimensions.defIt
    multiIndex(knowledge.Knowledge.dimensionality) = ir.IntegerConstant(arrayIndex)
    ir.FieldAccess(
      knowledge.FieldSelection(
        knowledge.FieldCollection.getFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get,
        slot.progressToIr,
        arrayIndex),
      multiIndex)
  }
}

case class StencilAccess(var name : String, var level : AccessLevelSpecification) extends Access {
  def progressToIr : ir.StencilAccess = {
    ir.StencilAccess(knowledge.StencilCollection.getStencilByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get)
  }
}

case class StencilFieldAccess(var name : String, var level : AccessLevelSpecification, var slot : Expression) extends Access {
  def progressToIr : ir.StencilFieldAccess = {
    ir.StencilFieldAccess(
      knowledge.StencilFieldSelection(
        knowledge.StencilFieldCollection.getStencilFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get,
        slot.progressToIr,
        -1),
      ir.LoopOverDimensions.defIt)
  }
}

abstract class Identifier() extends Expression { var name : String }

case class BasicIdentifier(var name : String) extends Identifier {
  def progressToIr : ir.StringConstant = ir.StringConstant(name)
}

case class LeveledIdentifier(var name : String, var level : LevelSpecification) extends Identifier {
  def progressToIr : ir.StringConstant = {
    ir.StringConstant(name + "_" + level.asInstanceOf[SingleLevelSpecification].level)
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

case class FunctionCallExpression(var identifier : Access, var arguments : List[Expression]) extends Expression {
  def progressToIr : ir.FunctionCallExpression = {
    ir.FunctionCallExpression(ir.StringConstant(identifier.progressToIr.asInstanceOf[ir.StringConstant].value),
      arguments.map(s => s.progressToIr).to[ListBuffer])
  }
}
