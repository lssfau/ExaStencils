package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils._
import exastencils.base.ir._
import exastencils.base.l4._
import exastencils.baseExt.ir._
import exastencils.datastructures._
import exastencils.logger._
import exastencils.prettyprinting._

trait Number extends L4_Expression {
  def value : AnyVal
}

case class VectorExpression(var datatype : Option[L4_Datatype], var expressions : List[L4_Expression], var rowVector : Option[Boolean]) extends L4_Expression {
  // rowVector == true: Row; false: Column; None: unspecified
  def length = expressions.length

  def apply(i : Integer) = expressions(i)
  def isConstant = expressions.filter(e => e.isInstanceOf[Number]).length == expressions.length

  def prettyprint(out : PpStream) = {
    out << '{' <<< (expressions, ", ") << '}'
    if (rowVector.getOrElse(true) == false) {
      out << 'T';
    }
  }
  def progress = new ir.VectorExpression(if (datatype.isDefined) Some(datatype.get.progress); else None, expressions.map(_.progress).to[ListBuffer], rowVector)
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

case class MatrixExpression(var datatype : Option[L4_Datatype], var expressions : List[VectorExpression]) extends L4_Expression {
  if (expressions.filter(x => x.length != expressions(0).length).length > 0) {
    Logger.error("Rows of matrix must be of equal length")
  }

  def prettyprint(out : PpStream) = { out << '{'; expressions.foreach(e => { e.prettyprint(out); out << ",\n" }); out << "} '" }

  def progress = new ir.MatrixExpression(if (datatype.isDefined) Some(datatype.get.progress); else None, expressions.map(_.expressions.map(_.progress).to[ListBuffer]).to[ListBuffer])

  def rows = expressions.length
  def columns = expressions(0).length
  def isConstant = expressions.filter(_.isConstant).length == expressions.length
}

abstract class Access() extends L4_Expression {
  def name : String
}

case class UnresolvedAccess(var name : String,
    var slot : Option[SlotModifier],
    var level : Option[AccessLevelSpecification],
    var offset : Option[L4_ExpressionIndex],
    var arrayIndex : Option[Int],
    var dirAccess : Option[L4_ExpressionIndex]) extends Access {
  def prettyprint(out : PpStream) = {
    out << name
    if (slot.isDefined) out << '[' << slot.get << ']'
    if (level.isDefined) out << '@' << level.get
    if (offset.isDefined) out << '@' << offset.get
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (dirAccess.isDefined) out << ':' << dirAccess
  }

  def progress : IR_StringLiteral = IR_StringLiteral("ERROR - Unresolved Access")

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
      FieldAccess(name, level.get, slot.getOrElse(SlotModifier.Active), arrayIndex, offset)
    } catch {
      case e : Exception => Logger.warn(s"""Could not resolve field "${ name }""""); throw e
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
    StencilFieldAccess(name, level.get, slot.getOrElse(SlotModifier.Active), arrayIndex, offset, dirAccess)
  }
}

case class BasicAccess(var name : String) extends Access {
  def prettyprint(out : PpStream) = { out << name }
  def progress : IR_StringLiteral = { IR_StringLiteral(name) }
}

case class LeveledAccess(var name : String, var level : AccessLevelSpecification) extends Access {
  def prettyprint(out : PpStream) = { out << name << '[' << level << ']' }

  def progress : IR_Expression = {
    IR_StringLiteral(name + "_" + level.asInstanceOf[SingleLevelSpecification].level)
  }
}

case class FieldAccess(var name : String, var level : AccessLevelSpecification, var slot : SlotModifier, var arrayIndex : Option[Int] = None, var offset : Option[L4_ExpressionIndex] = None) extends Access {
  def prettyprint(out : PpStream) = {
    // FIXME: omit slot if numSlots of target field is 1
    out << name << '[' << slot << ']' << '@' << level
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (offset.isDefined) out << "@" << offset
  }

  def progressNameToIr = {
    IR_StringLiteral(name + "_" + level.asInstanceOf[SingleLevelSpecification].level)
  }

  def resolveField : knowledge.Field = {
    knowledge.FieldCollection.getFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get
  }

  def progress : IR_FieldAccess = {
    // TODO: extract common index stuff from here and VirtualFieldAccess, StencilFieldAccess, etc
    var numDims = knowledge.Knowledge.dimensionality // TODO: resolve field info
    if (arrayIndex.isDefined) numDims += 1 // TODO: remove array index and update function after integration of vec types
    var multiIndex = IR_LoopOverDimensions.defIt(numDims)
    if (arrayIndex.isDefined)
      multiIndex(numDims - 1) = IR_IntegerConstant(arrayIndex.get)
    if (offset.isDefined) {
      var progressedOffset = offset.get.progress
      while (progressedOffset.indices.length < numDims) progressedOffset.indices :+= IR_IntegerConstant(0)
      multiIndex += progressedOffset
    }

    val field = knowledge.FieldCollection.getFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get
    IR_FieldAccess(knowledge.FieldSelection(field, IR_IntegerConstant(field.level), FieldAccess.resolveSlot(field, slot), arrayIndex), multiIndex)
  }
}

case class VirtualFieldAccess(var name : String, var level : AccessLevelSpecification, var arrayIndex : Option[Int] = None, var offset : Option[L4_ExpressionIndex] = None) extends Access {
  def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
    if (offset.isDefined) out << "@" << offset
  }

  def progress : ir.VirtualFieldAccess = {
    var numDims = knowledge.Knowledge.dimensionality // TODO: resolve field info
    if (arrayIndex.isDefined) numDims += 1 // TODO: remove array index and update function after integration of vec types
    var multiIndex = IR_LoopOverDimensions.defIt(numDims)
    if (arrayIndex.isDefined)
      multiIndex(numDims - 1) = IR_IntegerConstant(arrayIndex.get)
    if (offset.isDefined) {
      var progressedOffset = offset.get.progress
      while (progressedOffset.indices.length < numDims) progressedOffset.indices :+= IR_IntegerConstant(0)
      multiIndex += progressedOffset
    }

    ir.VirtualFieldAccess(name, IR_IntegerConstant(level.asInstanceOf[SingleLevelSpecification].level), multiIndex, arrayIndex)
  }
}

object FieldAccess {
  def resolveSlot(field : knowledge.Field, slot : SlotModifier) = {
    if (1 == field.numSlots) IR_IntegerConstant(0)
    else slot match {
      case SlotModifier.Active       => data.SlotAccess(ir.iv.CurrentSlot(field), 0)
      case SlotModifier.Next         => data.SlotAccess(ir.iv.CurrentSlot(field), 1)
      case SlotModifier.Previous     => data.SlotAccess(ir.iv.CurrentSlot(field), -1)
      case x : SlotModifier.Constant => IR_IntegerConstant(x.number)
      case _                         => Logger.error("Unknown slot modifier " + slot)
    }
  }
}

case class StencilAccess(var name : String, var level : AccessLevelSpecification, var arrayIndex : Option[Int] = None, var dirAccess : Option[L4_ExpressionIndex] = None) extends Access {
  def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (dirAccess.isDefined) out << ":" << dirAccess
  }

  def getBasicStencilAccess : ir.StencilAccess = {
    if (arrayIndex.isDefined || dirAccess.isDefined)
      Logger.warn(s"Discarding modifiers of access to stencil $name on level ${ level.asInstanceOf[SingleLevelSpecification].level }")

    ir.StencilAccess(knowledge.StencilCollection.getStencilByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get)
  }

  def progress : IR_Expression = {
    if (arrayIndex.isDefined && dirAccess.isDefined)
      Logger.warn(s"Access to stencil $name on level ${ level.asInstanceOf[SingleLevelSpecification].level } has dirAccess and array subscript modifiers; array index will be given precendence, dirAccess will be ignored")

    val stencil = knowledge.StencilCollection.getStencilByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get

    if (arrayIndex.isDefined)
      stencil.entries(arrayIndex.get).coefficient
    else if (dirAccess.isDefined)
      stencil.findStencilEntry(dirAccess.get.progress).get.coefficient
    else
      ir.StencilAccess(stencil)
  }
}

case class StencilFieldAccess(var name : String,
    var level : AccessLevelSpecification,
    var slot : SlotModifier,
    var arrayIndex : Option[Int] = None,
    var offset : Option[L4_ExpressionIndex] = None,
    var dirAccess : Option[L4_ExpressionIndex] = None) extends Access {
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
      Logger.warn(s"Discarding modifiers of access to stencilfield $name on level ${ level.asInstanceOf[SingleLevelSpecification].level }")

    val stencilField = knowledge.StencilFieldCollection.getStencilFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get

    var numDims = stencilField.field.fieldLayout.numDimsGrid
    if (arrayIndex.isDefined) numDims += 1 // TODO: remove array index and update function after integration of vec types
    var multiIndex = IR_LoopOverDimensions.defIt(numDims)
    if (arrayIndex.isDefined)
      multiIndex(numDims - 1) = IR_IntegerConstant(arrayIndex.get)
    if (offset.isDefined) {
      var progressedOffset = offset.get.progress
      while (progressedOffset.indices.length < numDims) progressedOffset.indices :+= IR_IntegerConstant(0)
      multiIndex += progressedOffset
    }

    ir.StencilFieldAccess(knowledge.StencilFieldSelection(stencilField, IR_IntegerConstant(stencilField.field.level), FieldAccess.resolveSlot(stencilField.field, slot), None), multiIndex)
  }

  def progress : IR_Expression = {
    if (arrayIndex.isDefined && dirAccess.isDefined)
      Logger.warn(s"Access to stencilfield $name on level ${ level.asInstanceOf[SingleLevelSpecification].level } has direction access and array subscript modifiers; array index will be given precendence, offset will be ignored")

    val stencilField = knowledge.StencilFieldCollection.getStencilFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get

    var accessIndex = -1

    if (arrayIndex.isDefined)
      accessIndex = arrayIndex.get
    else if (dirAccess.isDefined)
      accessIndex = stencilField.stencil.findStencilEntryIndex(dirAccess.get.progress).get

    var numDims = knowledge.Knowledge.dimensionality // TODO: resolve field info
    numDims += 1 // TODO: remove array index and update function after integration of vec types
    var multiIndex = IR_LoopOverDimensions.defIt(numDims)

    if (accessIndex < 0)
      multiIndex(numDims - 1) = IR_IntegerConstant(0)
    else
      multiIndex(numDims - 1) = IR_IntegerConstant(accessIndex)

    if (offset.isDefined) {
      var progressedOffset = offset.get.progress
      while (progressedOffset.indices.length < numDims) progressedOffset.indices :+= IR_IntegerConstant(0)
      multiIndex += progressedOffset
    }

    if (accessIndex < 0)
      ir.StencilFieldAccess(knowledge.StencilFieldSelection(stencilField, IR_IntegerConstant(stencilField.field.level), FieldAccess.resolveSlot(stencilField.field, slot), None),
        multiIndex)
    else
      IR_FieldAccess(knowledge.FieldSelection(stencilField.field, IR_IntegerConstant(stencilField.field.level), FieldAccess.resolveSlot(stencilField.field, slot), Some(accessIndex)),
        multiIndex)
  }
}

abstract class Identifier extends Node {
  var name : String
  def fullName : String
}

case class BasicIdentifier(var name : String) extends Identifier {
  def prettyprint(out : PpStream) = { out << name }

  def fullName = name
}

case class LeveledIdentifier(var name : String, var level : LevelSpecification) extends Identifier {
  def prettyprint(out : PpStream) = { out << name << '@' << level }

  def fullName = name + "_" + level.prettyprint
}

case class VariableExpression(var access : Access, var datatype : L4_Datatype) extends L4_Expression {
  def prettyprint(out : PpStream) = access.prettyprint(out)

  def progress = IR_VariableAccess(access.name, datatype.progress)
}

case class UnaryExpression(var operator : String, var exp : L4_Expression) extends L4_Expression {
  def prettyprint(out : PpStream) = { out << operator << '(' << exp << ')' }

  def progress : IR_Expression = {
    IR_UnaryOperators.createExpression(operator, exp.progress)
  }
}

case class FunctionCallExpression(var identifier : Access, var arguments : List[L4_Expression]) extends L4_Expression {
  def prettyprint(out : PpStream) = { out << identifier << " ( " <<< (arguments, ", ") << " )" }

  def progress : IR_FunctionCall = {
    IR_FunctionCall(identifier.progress.asInstanceOf[IR_StringLiteral].value,
      arguments.map(s => s.progress).to[ListBuffer])
  }
}

case class StencilConvolution(var stencilAccess : StencilAccess, var fieldAccess : FieldAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = { out << stencilAccess << " * " << fieldAccess }

  def progress : ir.StencilConvolution = {
    ir.StencilConvolution(stencilAccess.getBasicStencilAccess.stencil, fieldAccess.progress)
  }
}

case class StencilFieldConvolution(var stencilFieldAccess : StencilFieldAccess, var fieldAccess : FieldAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = { out << stencilFieldAccess << " * " << fieldAccess }

  def progress : ir.StencilFieldConvolution = {
    ir.StencilFieldConvolution(stencilFieldAccess.getBasicStencilFieldAccess, fieldAccess.progress)
  }
}

case class StencilStencilConvolution(var stencilLeft : StencilAccess, var stencilRight : StencilAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = { out << stencilLeft << " * " << stencilRight }

  def progress : ir.StencilStencilConvolution = {
    ir.StencilStencilConvolution(stencilLeft.getBasicStencilAccess.stencil, stencilRight.getBasicStencilAccess.stencil)
  }
}

case class StencilFieldStencilConvolution(var stencilLeft : StencilFieldAccess, var stencilRight : StencilAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = { out << stencilLeft << " * " << stencilRight }

  def progress : ir.StencilFieldStencilConvolution = {
    ir.StencilFieldStencilConvolution(stencilLeft.getBasicStencilFieldAccess, stencilRight.getBasicStencilAccess.stencil)
  }
}
