package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils.data
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
  def resolveToFieldAccess = FieldAccess(identifier, level.get, slot.getOrElse(IntegerConstant(0)), arrayIndex.getOrElse(-1))
  def resolveToStencilAccess = StencilAccess(identifier, level.get)
  def resolveToStencilFieldAccess = StencilFieldAccess(identifier, level.get, slot.getOrElse(IntegerConstant(0)), arrayIndex.getOrElse(-1))
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
    multiIndex(knowledge.Knowledge.dimensionality) = ir.IntegerConstant(if (arrayIndex >= 0) arrayIndex else 0)

    val field = knowledge.FieldCollection.getFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get
    ir.FieldAccess(knowledge.FieldSelection(field, ir.IntegerConstant(field.level), FieldAccess.resolveSlot(field, slot), arrayIndex), multiIndex)
  }
}

object FieldAccess {
  def resolveSlot(field : knowledge.Field, slot : Expression) = {
    if (1 == field.numSlots) ir.IntegerConstant(0) else slot match {
      // TODO: these keywords are up to discussion
      // TODO: detect these keywords directly in the parser? Add specialized node(s)?
      case BasicAccess("curSlot")  => data.SlotAccess(ir.iv.CurrentSlot(field), 0)
      case BasicAccess("nextSlot") => data.SlotAccess(ir.iv.CurrentSlot(field), 1)
      case BasicAccess("prevSlot") => data.SlotAccess(ir.iv.CurrentSlot(field), -1)
      case _                       => slot.progressToIr
    }
  }
}

case class StencilAccess(var name : String, var level : AccessLevelSpecification) extends Access {
  def progressToIr : ir.StencilAccess = {
    ir.StencilAccess(knowledge.StencilCollection.getStencilByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get)
  }
}

case class StencilFieldAccess(var name : String, var level : AccessLevelSpecification, var slot : Expression, var arrayIndex : Int) extends Access {
  def progressToIr : ir.StencilFieldAccess = {
    val field = knowledge.StencilFieldCollection.getStencilFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get
    ir.StencilFieldAccess(knowledge.StencilFieldSelection(field, ir.IntegerConstant(field.field.level), FieldAccess.resolveSlot(field.field, slot), arrayIndex), ir.LoopOverDimensions.defIt)
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

case class UnaryBooleanExpression(var operator : String, var exp : Expression) extends Expression {
  def progressToIr : ir.Expression = {
    ir.BinaryOperators.CreateExpression(operator, exp.progressToIr, null) // second argument is ignored
  }
}

case class FunctionCallExpression(var identifier : Access, var arguments : List[Expression]) extends Expression {
  def progressToIr : ir.FunctionCallExpression = {
    ir.FunctionCallExpression(ir.StringConstant(identifier.progressToIr.asInstanceOf[ir.StringConstant].value),
      arguments.map(s => s.progressToIr).to[ListBuffer])
  }
}
