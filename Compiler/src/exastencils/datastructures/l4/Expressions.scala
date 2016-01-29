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
  def this(s : l4.StringLiteral) = this(s.value)
  def prettyprint(out : PpStream) = { out << '\'' << value << '\'' }
  def progressToIr = ir.StringConstant(value)
}

case class StringLiteral(var value : String) extends Expression {
  def this(s : l4.StringConstant) = this(s.value)
  def prettyprint(out : PpStream) = { out << '"' << value << '"' }
  def progressToIr = ir.StringLiteral(value)
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

case class VectorExpression(var datatype : Option[Datatype], var expressions : List[Expression], var rowVector : Option[Boolean]) extends Expression {
  // rowVector == true: Row; false: Column; None: unspecified
  def length = expressions.length

  def apply(i : Integer) = expressions(i)
  def isConstant = expressions.filter(e => e.isInstanceOf[Number]).length == expressions.length

  def prettyprint(out : PpStream) = {
    out << '{'
    expressions.mkString(", ");
    out << '}'
    if (rowVector.getOrElse(true) == false) {
      out << 'T';
    }
  }
  def progressToIr = new ir.VectorExpression(if (datatype.isDefined) Some(datatype.get.progressToIr); else None, expressions.map(_.progressToIr).to[ListBuffer], rowVector)
}

object VectorExpression {
  // helper function
  def isRowVector(n : Node) = {
    if (n.isInstanceOf[VectorExpression]) {
      var v = n.asInstanceOf[VectorExpression]
      if (v.rowVector.getOrElse(true)) true; else false
    } else {
      false
    }
  }
  def isColumnVector(n : Node) = {
    if (n.isInstanceOf[VectorExpression]) {
      var v = n.asInstanceOf[VectorExpression]
      if (v.rowVector.getOrElse(true)) true; else false
    } else {
      false
    }
  }
}

case class MatrixExpression(var datatype : Option[Datatype], var expressions : List[VectorExpression]) extends Expression {
  if (expressions.filter(x => x.length != expressions(0).length).length > 0) {
    Logger.error("Rows of matrix must be of equal length")
  }

  def prettyprint(out : PpStream) = { out << '{'; expressions.foreach(e => { e.prettyprint(out); out << ",\n" }); out << "} '" }

  def progressToIr = new ir.MatrixExpression(if (datatype.isDefined) Some(datatype.get.progressToIr); else None, expressions.map(_.expressions.map(_.progressToIr).to[ListBuffer]).to[ListBuffer])

  def rows = expressions.length
  def columns = expressions(0).length
  def isConstant = expressions.filter(_.isConstant).length == expressions.length
}

abstract class Access(var name : String) extends Expression {

}

case class UnresolvedAccess(name_ : String,
    var slot : Option[SlotModifier],
    var level : Option[AccessLevelSpecification],
    var offset : Option[ExpressionIndex],
    var arrayIndex : Option[Int],
    var dirAccess : Option[ExpressionIndex]) extends Access(name_) {
  def prettyprint(out : PpStream) = {
    out << name
    if (slot.isDefined) out << '[' << slot.get << ']'
    if (level.isDefined) out << '@' << level.get
    if (offset.isDefined) out << '@' << offset.get
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (dirAccess.isDefined) out << ':' << dirAccess
  }

  def progressToIr : ir.StringLiteral = ir.StringLiteral("ERROR - Unresolved Access")

  def resolveToBasicOrLeveledAccess = {
    if (slot.isDefined) Logger.warn("Discarding meaningless slot access on basic or leveled access")
    if (offset.isDefined) Logger.warn("Discarding meaningless offset access on basic or leveled access")
    if (arrayIndex.isDefined) Logger.warn("Discarding meaningless array index access on basic or leveled access")
    if (dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on basic or leveled access")
    if (level.isDefined) LeveledAccess(name, level.get) else BasicAccess(name)
  }
  def resolveToFieldAccess = {
    if (dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on field - was an offset access (@) intended?")
    try {
      FieldAccess(name, level.get, slot.getOrElse(SlotModifier.Active()), arrayIndex, offset)
    } catch {
      case e : Exception => Logger.warn(s"""Could not resolve field "${name}""""); throw e
    }
  }
  def resolveToVirtualFieldAccess = {
    if (dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on special field - was an offset access (@) intended?")
    if (slot.isDefined) Logger.warn("Discarding meaningless slot access on special field")
    VirtualFieldAccess(name, level.get, arrayIndex, offset)
  }
  def resolveToStencilAccess = {
    if (slot.isDefined) Logger.warn("Discarding meaningless slot access on stencil")
    if (offset.isDefined) Logger.warn("Discarding meaningless offset access on stencil - was a direction access (:) intended?")
    StencilAccess(name, level.get, arrayIndex, dirAccess)
  }
  def resolveToStencilFieldAccess = {
    StencilFieldAccess(name, level.get, slot.getOrElse(SlotModifier.Active()), arrayIndex, offset, dirAccess)
  }
}

case class BasicAccess(name_ : String) extends Access(name_) {
  def prettyprint(out : PpStream) = { out << name }

  def progressToIr : ir.StringLiteral = ir.StringLiteral(name)
}

case class LeveledAccess(name_ : String, var level : AccessLevelSpecification) extends Access(name_) {
  def prettyprint(out : PpStream) = { out << name << '[' << level << ']' }

  def progressToIr : ir.Expression = {
    ir.StringLiteral(name + "_" + level.asInstanceOf[SingleLevelSpecification].level)
  }
}

case class FieldAccess(name_ : String, var level : AccessLevelSpecification, var slot : SlotModifier, var arrayIndex : Option[Int] = None, var offset : Option[ExpressionIndex] = None) extends Access(name_) {
  def prettyprint(out : PpStream) = {
    // FIXME: omit slot if numSlots of target field is 1
    out << name << '[' << slot << ']' << '@' << level
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (offset.isDefined) out << "@" << offset
  }

  def progressNameToIr = {
    ir.StringLiteral(name + "_" + level.asInstanceOf[SingleLevelSpecification].level)
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

case class VirtualFieldAccess(name_ : String, var level : AccessLevelSpecification, var arrayIndex : Option[Int] = None, var offset : Option[ExpressionIndex] = None) extends Access(name_) {
  def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (offset.isDefined) out << "@" << offset
  }

  def progressToIr : ir.VirtualFieldAccess = {
    var multiIndex = ir.LoopOverDimensions.defIt
    if (offset.isDefined) multiIndex += offset.get.progressToIr
    multiIndex(knowledge.Knowledge.dimensionality) = ir.IntegerConstant(arrayIndex.getOrElse(0).toLong)

    ir.VirtualFieldAccess(name, ir.IntegerConstant(level.asInstanceOf[SingleLevelSpecification].level), multiIndex, arrayIndex)
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

case class StencilAccess(name_ : String, var level : AccessLevelSpecification, var arrayIndex : Option[Int] = None, var dirAccess : Option[ExpressionIndex] = None) extends Access(name_) {
  def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (dirAccess.isDefined) out << ":" << dirAccess
  }

  def getBasicStencilAccess : ir.StencilAccess = {
    if (arrayIndex.isDefined || dirAccess.isDefined)
      Logger.warn(s"Discarding modifiers of access to stencil $name on level ${level.asInstanceOf[SingleLevelSpecification].level}")

    ir.StencilAccess(knowledge.StencilCollection.getStencilByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get)
  }

  def progressToIr : ir.Expression = {
    if (arrayIndex.isDefined && dirAccess.isDefined)
      Logger.warn(s"Access to stencil $name on level ${level.asInstanceOf[SingleLevelSpecification].level} has dirAccess and array subscript modifiers; array index will be given precendence, dirAccess will be ignored")

    val stencil = knowledge.StencilCollection.getStencilByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get

    if (arrayIndex.isDefined)
      stencil.entries(arrayIndex.get).coefficient
    else if (dirAccess.isDefined)
      stencil.findStencilEntry(dirAccess.get.progressToIr).get.coefficient
    else
      ir.StencilAccess(stencil)
  }
}

case class StencilFieldAccess(name_ : String,
    var level : AccessLevelSpecification,
    var slot : SlotModifier,
    var arrayIndex : Option[Int] = None,
    var offset : Option[ExpressionIndex] = None,
    var dirAccess : Option[ExpressionIndex] = None) extends Access(name_) {
  def prettyprint(out : PpStream) = {
    // FIXME: omit slot if numSlots of target field is 1
    out << name << '[' << slot << ']' << '@' << level
    if (offset.isDefined) out << "@" << offset
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (dirAccess.isDefined) out << ":" << dirAccess
  }

  def resolveField : knowledge.Field = {
    knowledge.StencilFieldCollection.getStencilFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get.field
  }

  def getBasicStencilFieldAccess : ir.StencilFieldAccess = {
    if (arrayIndex.isDefined || dirAccess.isDefined)
      Logger.warn(s"Discarding modifiers of access to stencilfield $name on level ${level.asInstanceOf[SingleLevelSpecification].level}")

    val stencilField = knowledge.StencilFieldCollection.getStencilFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get
    var multiIndex = ir.LoopOverDimensions.defIt
    if (offset.isDefined) multiIndex += offset.get.progressToIr
    multiIndex(knowledge.Knowledge.dimensionality) = ir.IntegerConstant(arrayIndex.getOrElse(0).toLong)

    ir.StencilFieldAccess(knowledge.StencilFieldSelection(stencilField, ir.IntegerConstant(stencilField.field.level), FieldAccess.resolveSlot(stencilField.field, slot), None), multiIndex)
  }

  def progressToIr : ir.Expression = {
    if (arrayIndex.isDefined && dirAccess.isDefined)
      Logger.warn(s"Access to stencilfield $name on level ${level.asInstanceOf[SingleLevelSpecification].level} has direction access and array subscript modifiers; array index will be given precendence, offset will be ignored")

    val stencilField = knowledge.StencilFieldCollection.getStencilFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get

    var multiIndex = ir.LoopOverDimensions.defIt
    if (offset.isDefined) multiIndex += offset.get.progressToIr

    var accessIndex = -1

    if (arrayIndex.isDefined)
      accessIndex = arrayIndex.get
    else if (dirAccess.isDefined)
      accessIndex = stencilField.stencil.findStencilEntryIndex(dirAccess.get.progressToIr).get

    if (accessIndex < 0) {
      multiIndex(knowledge.Knowledge.dimensionality) = ir.IntegerConstant(0)
      ir.StencilFieldAccess(knowledge.StencilFieldSelection(stencilField, ir.IntegerConstant(stencilField.field.level), FieldAccess.resolveSlot(stencilField.field, slot), None), multiIndex)
    } else {
      multiIndex(knowledge.Knowledge.dimensionality) = ir.IntegerConstant(accessIndex)
      ir.FieldAccess(knowledge.FieldSelection(stencilField.field, ir.IntegerConstant(stencilField.field.level), FieldAccess.resolveSlot(stencilField.field, slot), Some(accessIndex)),
        multiIndex)
    }
  }
}

abstract class Identifier extends Expression {
  var name : String
}

case class BasicIdentifier(var name : String) extends Identifier {
  def prettyprint(out : PpStream) = { out << name }

  def progressToIr = ir.StringLiteral(name)
}

case class LeveledIdentifier(var name : String, var level : LevelSpecification) extends Identifier {
  def prettyprint(out : PpStream) = { out << name << '@' << level }

  def progressToIr = {
    ir.StringLiteral(name + "_" + level.asInstanceOf[SingleLevelSpecification].level)
  }
}

case class Variable(var identifier : Identifier, var datatype : Datatype) extends Expression {
  def prettyprint(out : PpStream) = { out << identifier }

  def progressToIr = {
    ir.VariableAccess(identifier.progressToIr.asInstanceOf[ir.StringLiteral].value, Some(datatype.progressToIr))
  }
}

case class UnaryExpression(var operator : String, var exp : Expression) extends Expression {
  def prettyprint(out : PpStream) = { out << operator << '(' << exp << ')' }

  def progressToIr : ir.Expression = {
    ir.UnaryOperators.CreateExpression(operator, exp.progressToIr)
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
    ir.FunctionCallExpression(identifier.progressToIr.asInstanceOf[ir.StringLiteral].value,
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
