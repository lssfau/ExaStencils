package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.data
import exastencils.datastructures._
import exastencils.knowledge
import exastencils.logger._
import exastencils.prettyprinting._

trait Expression extends Node with ProgressableToIr with PrettyPrintable {
  def progressToIr : ir.Expression
}

trait Number extends Expression {
  def value : AnyVal
}

case class StringConstant(var value : String) extends Expression {
  def prettyprint(out : PpStream) = { out << '\'' << value << '\'' }

  def progressToIr : ir.StringConstant = ir.StringConstant(value)
}

case class IntegerConstant(var v : Long) extends Number {
  override def value = v

  def prettyprint(out : PpStream) = { out << v }

  def progressToIr : ir.IntegerConstant = ir.IntegerConstant(v)
}

case class FloatConstant(var v : Double) extends Number {
  override def value = v

  def prettyprint(out : PpStream) = {
    out << String.format(java.util.Locale.US, "%s", Double.box(value)) // ensure the compiler can parse the string
  }

  def progressToIr : ir.FloatConstant = ir.FloatConstant(v)
}

case class BooleanConstant(var value : Boolean) extends Expression {
  def prettyprint(out : PpStream) = { out << value }

  def progressToIr : ir.BooleanConstant = ir.BooleanConstant(value)
}

abstract class Access() extends Expression {
  var name : String
}

case class UnresolvedAccess(var name : String, var level : Option[AccessLevelSpecification], var slot : Option[SlotModifier], var arrayIndex : Option[Int], var offset : Option[ExpressionIndex]) extends Access {
  def prettyprint(out : PpStream) = {
    out << name
    if (slot.isDefined) out << '[' << slot.get << ']'
    if (level.isDefined) out << '@' << level.get
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (offset.isDefined) out << ':' << offset.get
  }

  def progressToIr : ir.StringConstant = ir.StringConstant("ERROR - Unresolved Access")

  def resolveToBasicOrLeveledAccess = if (level.isDefined) LeveledAccess(name, level.get) else BasicAccess(name)
  def resolveToFieldAccess = FieldAccess(name, level.get, slot.getOrElse(SlotModifier.Active()), arrayIndex, offset)
  def resolveToStencilAccess = StencilAccess(name, level.get, arrayIndex, offset)
  def resolveToStencilFieldAccess = StencilFieldAccess(name, level.get, slot.getOrElse(SlotModifier.Active()), arrayIndex, offset)
}

case class BasicAccess(var name : String) extends Access {
  def prettyprint(out : PpStream) = { out << name }

  def progressToIr : ir.StringConstant = ir.StringConstant(name)
}

case class LeveledAccess(var name : String, var level : AccessLevelSpecification) extends Access {
  def prettyprint(out : PpStream) = { out << name << '[' << level << ']' }

  def progressToIr : ir.Expression = {
    if ("levels" == name) // TODO: incorporate this into the parser?
      ir.IntegerConstant(level.asInstanceOf[SingleLevelSpecification].level)
    else if ("levelStrings" == name)
      ir.StringConstant(level.asInstanceOf[SingleLevelSpecification].level.toString)
    else
      ir.StringConstant(name + "_" + level.asInstanceOf[SingleLevelSpecification].level)
  }
}

case class FieldAccess(var name : String, var level : AccessLevelSpecification, var slot : SlotModifier, var arrayIndex : Option[Int] = None, var offset : Option[ExpressionIndex] = None) extends Access {
  def prettyprint(out : PpStream) = {
    // FIXME: omit slot if numSlots of target field is 1
    out << name << '[' << slot << ']' << '@' << level
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (offset.isDefined) out << ":" << offset
  }

  def progressNameToIr : ir.StringConstant = {
    ir.StringConstant(name + "_" + level.asInstanceOf[SingleLevelSpecification].level)
  }

  def resolveField : knowledge.Field = {
    knowledge.FieldCollection.getFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get
  }

  def progressToIr : ir.FieldAccess = {
    var multiIndex = ir.LoopOverDimensions.defIt
    if (offset.isDefined) multiIndex += offset.get.progressToIr
    multiIndex(knowledge.Knowledge.dimensionality) = ir.IntegerConstant(arrayIndex.getOrElse(0).toLong)

    val field = knowledge.FieldCollection.getFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get
    ir.FieldAccess(knowledge.FieldSelection(field, ir.IntegerConstant(field.level), FieldAccess.resolveSlot(field, slot), arrayIndex), multiIndex)
  }
}

object FieldAccess {
  def resolveSlot(field : knowledge.Field, slot : SlotModifier) = {
    if (1 == field.numSlots) ir.IntegerConstant(0) else slot match {
      case x : SlotModifier.Active   => data.SlotAccess(ir.iv.CurrentSlot(field), 0)
      case x : SlotModifier.Next     => data.SlotAccess(ir.iv.CurrentSlot(field), 1)
      case x : SlotModifier.Previous => data.SlotAccess(ir.iv.CurrentSlot(field), -1)
      case x : SlotModifier.Constant => ir.IntegerConstant(x.number)
      case _                         => Logger.error("Unknown slot modifier " + slot)
    }
  }
}

case class StencilAccess(var name : String, var level : AccessLevelSpecification, var arrayIndex : Option[Int] = None, var offset : Option[ExpressionIndex] = None) extends Access {
  def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << ":" << offset
  }

  def getBasicStencilAccess : ir.StencilAccess = {
    if (arrayIndex.isDefined || offset.isDefined)
      Logger.warn(s"Discarding modifiers of access to stencil $name on level ${level.asInstanceOf[SingleLevelSpecification].level}")

    ir.StencilAccess(knowledge.StencilCollection.getStencilByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get)
  }

  def progressToIr : ir.Expression = {
    if (arrayIndex.isDefined && offset.isDefined)
      Logger.warn(s"Access to stencil $name on level ${level.asInstanceOf[SingleLevelSpecification].level} has offset and array subscript modifiers; array index will be given precendence, offset will be ignored")

    val stencil = knowledge.StencilCollection.getStencilByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get

    if (arrayIndex.isDefined)
      stencil.entries(arrayIndex.get).coefficient
    else if (offset.isDefined)
      stencil.findStencilEntry(offset.get.progressToIr).get.coefficient
    else
      ir.StencilAccess(stencil)
  }
}

case class StencilFieldAccess(var name : String, var level : AccessLevelSpecification, var slot : SlotModifier, var arrayIndex : Option[Int] = None, var offset : Option[ExpressionIndex] = None) extends Access {
  def prettyprint(out : PpStream) = {
    // FIXME: omit slot if numSlots of target field is 1
    out << name << '[' << slot << ']' << '@' << level
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (offset.isDefined) out << ":" << offset
  }

  def getBasicStencilFieldAccess : ir.StencilFieldAccess = {
    if (arrayIndex.isDefined || offset.isDefined)
      Logger.warn(s"Discarding modifiers of access to stencilfield $name on level ${level.asInstanceOf[SingleLevelSpecification].level}")

    val stencilField = knowledge.StencilFieldCollection.getStencilFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get
    ir.StencilFieldAccess(knowledge.StencilFieldSelection(stencilField, ir.IntegerConstant(stencilField.field.level), FieldAccess.resolveSlot(stencilField.field, slot), arrayIndex), ir.LoopOverDimensions.defIt)
  }

  def progressToIr : ir.Expression = {
    if (arrayIndex.isDefined && offset.isDefined)
      Logger.warn(s"Access to stencilfield $name on level ${level.asInstanceOf[SingleLevelSpecification].level} has offset and array subscript modifiers; array index will be given precendence, offset will be ignored")

    val stencilField = knowledge.StencilFieldCollection.getStencilFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get

    if (arrayIndex.isDefined) {
      var multiIndex = ir.LoopOverDimensions.defIt
      multiIndex(knowledge.Knowledge.dimensionality) = ir.IntegerConstant(arrayIndex.get)

      ir.FieldAccess(knowledge.FieldSelection(stencilField.field, ir.IntegerConstant(stencilField.field.level), FieldAccess.resolveSlot(stencilField.field, slot), arrayIndex),
        multiIndex)
    } else if (offset.isDefined) {
      val index = stencilField.stencil.findStencilEntryIndex(offset.get.progressToIr).get

      var multiIndex = ir.LoopOverDimensions.defIt
      multiIndex(knowledge.Knowledge.dimensionality) = ir.IntegerConstant(index)

      ir.FieldAccess(knowledge.FieldSelection(stencilField.field, ir.IntegerConstant(stencilField.field.level), FieldAccess.resolveSlot(stencilField.field, slot), Some(index)),
        multiIndex)
    } else {
      ir.StencilFieldAccess(knowledge.StencilFieldSelection(stencilField, ir.IntegerConstant(stencilField.field.level), FieldAccess.resolveSlot(stencilField.field, slot), arrayIndex), ir.LoopOverDimensions.defIt)
    }
  }
}

abstract class Identifier extends Expression {
  var name : String
}

case class BasicIdentifier(var name : String) extends Identifier {
  def prettyprint(out : PpStream) = { out << name }

  def progressToIr : ir.StringConstant = ir.StringConstant(name)
}

case class LeveledIdentifier(var name : String, var level : LevelSpecification) extends Identifier {
  def prettyprint(out : PpStream) = { out << name << '@' << level }

  def progressToIr : ir.StringConstant = {
    ir.StringConstant(name + "_" + level.asInstanceOf[SingleLevelSpecification].level)
  }
}

case class Variable(var identifier : Identifier, var datatype : Datatype) extends Expression {
  def prettyprint(out : PpStream) = { out << identifier }

  def progressToIr : ir.VariableAccess = {
    ir.VariableAccess(identifier.progressToIr.asInstanceOf[ir.StringConstant].value, Some(datatype.progressToIr))
  }
}

case class BinaryExpression(var operator : String, var left : Expression, var right : Expression) extends Expression {
  def prettyprint(out : PpStream) = { out << '(' << left << ' ' << operator << ' ' << right << ')' }

  def progressToIr : ir.Expression = {
    this match {
      case BinaryExpression("**", left, IntegerConstant(1)) => left.progressToIr
      case BinaryExpression("**", left, IntegerConstant(2)) => left.progressToIr * Duplicate(left).progressToIr
      case BinaryExpression("**", left, IntegerConstant(3)) => left.progressToIr * Duplicate(left).progressToIr * Duplicate(left).progressToIr
      case _ => ir.BinaryOperators.CreateExpression(operator, left.progressToIr, right.progressToIr)
    }
  }
}

case class BooleanExpression(var operator : String, var left : Expression, var right : Expression) extends Expression {
  def prettyprint(out : PpStream) = { out << '(' << left << ' ' << operator << ' ' << right << ')' }

  def progressToIr : ir.Expression = {
    ir.BinaryOperators.CreateExpression(operator, left.progressToIr, right.progressToIr)
  }
}

case class UnaryBooleanExpression(var operator : String, var exp : Expression) extends Expression {
  def prettyprint(out : PpStream) = { out << '(' << operator << ' ' << exp << ')' }

  def progressToIr : ir.Expression = {
    ir.BinaryOperators.CreateExpression(operator, exp.progressToIr, null) // second argument is ignored
  }
}

case class FunctionCallExpression(var identifier : Access, var arguments : List[Expression]) extends Expression {
  def prettyprint(out : PpStream) = { out << identifier << " ( " <<< (arguments, ", ") << " )" }

  def progressToIr : ir.FunctionCallExpression = {
    ir.FunctionCallExpression(identifier.progressToIr.asInstanceOf[ir.StringConstant].value,
      arguments.map(s => s.progressToIr).to[ListBuffer])
  }
}

case class StencilConvolution(var stencilAccess : StencilAccess, var fieldAccess : FieldAccess) extends Expression {
  def prettyprint(out : PpStream) = { out << stencilAccess << " * " << fieldAccess }

  def progressToIr : ir.StencilConvolution = {
    ir.StencilConvolution(stencilAccess.getBasicStencilAccess.stencil, fieldAccess.progressToIr)
  }
}

case class StencilFieldConvolution(var stencilFieldAccess : StencilFieldAccess, var fieldAccess : FieldAccess) extends Expression {
  def prettyprint(out : PpStream) = { out << stencilFieldAccess << " * " << fieldAccess }

  def progressToIr : ir.StencilFieldConvolution = {
    ir.StencilFieldConvolution(stencilFieldAccess.getBasicStencilFieldAccess, fieldAccess.progressToIr)
  }
}

case class StencilStencilConvolution(var stencilLeft : StencilAccess, var stencilRight : StencilAccess) extends Expression {
  def prettyprint(out : PpStream) = { out << stencilLeft << " * " << stencilRight }

  def progressToIr : ir.StencilStencilConvolution = {
    ir.StencilStencilConvolution(stencilLeft.getBasicStencilAccess.stencil, stencilRight.getBasicStencilAccess.stencil)
  }
}

case class StencilFieldStencilConvolution(var stencilLeft : StencilFieldAccess, var stencilRight : StencilAccess) extends Expression {
  def prettyprint(out : PpStream) = { out << stencilLeft << " * " << stencilRight }

  def progressToIr : ir.StencilFieldStencilConvolution = {
    ir.StencilFieldStencilConvolution(stencilLeft.getBasicStencilFieldAccess, stencilRight.getBasicStencilAccess.stencil)
  }
}
