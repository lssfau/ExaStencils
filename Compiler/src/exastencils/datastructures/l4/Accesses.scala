package exastencils.datastructures.l4

import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.data
import exastencils.datastructures._
import exastencils.knowledge
import exastencils.logger._
import exastencils.prettyprinting._

abstract class Access(var name : String, var componentIndex : Option[ComponentIndex]) extends Expression

case class UnresolvedAccess(
    name : String,
    var slot : Option[SlotModifier],
    var level : Option[AccessLevelSpecification],
    var offset : Option[ExpressionIndex],
    componentIndex : Option[ComponentIndex],
    var dirAccess : Option[ExpressionIndex]) extends Access(name, componentIndex) {
  def prettyprint(out : PpStream) = {
    out << name
    if (slot.isDefined) out << '[' << slot.get << ']'
    if (level.isDefined) out << '@' << level.get
    if (offset.isDefined) out << '@' << offset.get
    if (componentIndex.isDefined) componentIndex.get.prettyprint(out)
    if (dirAccess.isDefined) out << ':' << dirAccess
  }

  def progressToIr : ir.StringLiteral = ir.StringLiteral("ERROR - Unresolved Access")

  def resolveToBasicOrLeveledAccess = {
    if (slot.isDefined) Logger.warn("Discarding meaningless slot access on basic or leveled access")
    if (offset.isDefined) Logger.warn("Discarding meaningless offset access on basic or leveled access")
    if (componentIndex.isDefined) Logger.warn("Discarding meaningless component index access on basic or leveled access")
    if (dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on basic or leveled access")
    if (level.isDefined) LeveledAccess(name, level.get, componentIndex) else BasicAccess(name, componentIndex)
  }
  def resolveToFieldAccess = {
    if (dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on field - was an offset access (@) intended?")
    try {
      FieldAccess(name, level.get, slot.getOrElse(SlotModifier.Active()), componentIndex, offset)
    } catch {
      case e : Exception => Logger.warn(s"""Could not resolve field "${name}""""); throw e
    }
  }
  def resolveToVirtualFieldAccess = {
    if (dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on special field - was an offset access (@) intended?")
    if (slot.isDefined) Logger.warn("Discarding meaningless slot access on special field")
    VirtualFieldAccess(name, level.get, componentIndex, offset)
  }
  def resolveToStencilAccess = {
    if (slot.isDefined) Logger.warn("Discarding meaningless slot access on stencil")
    if (offset.isDefined) Logger.warn("Discarding meaningless offset access on stencil - was a direction access (:) intended?")
    StencilAccess(name, level.get, componentIndex, dirAccess)
  }
  def resolveToStencilFieldAccess = {
    StencilFieldAccess(name, level.get, slot.getOrElse(SlotModifier.Active()), componentIndex, offset, dirAccess)
  }
}

case class BasicAccess(
    name : String,
    componentIndex : Option[ComponentIndex] = None) extends Access(name, componentIndex) {
  def prettyprint(out : PpStream) = {
    out << name
    if (componentIndex.isDefined) componentIndex.get.prettyprint(out)
  }

  def progressToIr : ir.StringLiteral = ir.StringLiteral(name)
}

case class LeveledAccess(
    name : String,
    var level : AccessLevelSpecification,
    componentIndex : Option[ComponentIndex] = None) extends Access(name,componentIndex) {
  def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if(componentIndex.isDefined) componentIndex.get.prettyprint(out)
  }

  def progressToIr : ir.Expression = {
    ir.StringLiteral(name + "_" + level.asInstanceOf[SingleLevelSpecification].level)
  }
}

case class FieldAccess(
    name : String,
    var level : AccessLevelSpecification,
    var slot : SlotModifier,
    componentIndex : Option[ComponentIndex] = None,
    var offset : Option[ExpressionIndex] = None) extends Access(name,componentIndex) {
  def prettyprint(out : PpStream) = {
    // FIXME: omit slot if numSlots of target field is 1
    out << name << '[' << slot << ']' << '@' << level
    if (componentIndex.isDefined) componentIndex.get.prettyprint(out)
    if (offset.isDefined) out << '@' << offset
  }

  def progressNameToIr = {
    ir.StringLiteral(name + "_" + level.asInstanceOf[SingleLevelSpecification].level)
  }

  def resolveField : knowledge.Field = {
    knowledge.FieldCollection.getFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get
  }

  def progressToIr : ir.FieldAccess = {
    // TODO: extract common index stuff from here and VirtualFieldAccess, StencilFieldAccess, etc
    var numDims = knowledge.Knowledge.dimensionality // TODO: resolve field info
    if (arrayIndex.isDefined) numDims += 1 // TODO: remove array index and update function after integration of vec types
    var multiIndex = ir.LoopOverDimensions.defIt(numDims)
    if (arrayIndex.isDefined)
      multiIndex(numDims - 1) = ir.IntegerConstant(arrayIndex.get)
    if (offset.isDefined) {
      var progressedOffset = offset.get.progressToIr
      while (progressedOffset.indices.length < numDims) progressedOffset.indices :+= ir.IntegerConstant(0)
      multiIndex += progressedOffset
    }

    val field = knowledge.FieldCollection.getFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get
    ir.FieldAccess(knowledge.FieldSelection(field, ir.IntegerConstant(field.level), FieldAccess.resolveSlot(field, slot), arrayIndex), multiIndex)
  }
}

case class VirtualFieldAccess(
    name : String,
    var level : AccessLevelSpecification,
    var componentIndex : Option[ComponentIndex] = None,
    var offset : Option[ExpressionIndex] = None) extends Access(name,componentIndex) {
  def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (componentIndex.isDefined) componentIndex.get.prettyprint(out)
    if (offset.isDefined) out << "@" << offset
  }

  def progressToIr : ir.VirtualFieldAccess = {
    var numDims = knowledge.Knowledge.dimensionality // TODO: resolve field info
    if (arrayIndex.isDefined) numDims += 1 // TODO: remove array index and update function after integration of vec types
    var multiIndex = ir.LoopOverDimensions.defIt(numDims)
    if (arrayIndex.isDefined)
      multiIndex(numDims - 1) = ir.IntegerConstant(arrayIndex.get)
    if (offset.isDefined) {
      var progressedOffset = offset.get.progressToIr
      while (progressedOffset.indices.length < numDims) progressedOffset.indices :+= ir.IntegerConstant(0)
      multiIndex += progressedOffset
    }

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

case class StencilAccess(
    name : String,
    var level : AccessLevelSpecification,
    var componentIndex : Option[ComponentIndex] = None,
    var dirAccess : Option[ExpressionIndex] = None) extends Access(name,componentIndex) {
  def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if(componentIndex.isDefined) componentIndex.get.prettyprint(out)
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

case class StencilFieldAccess(name : String,
                              var level : AccessLevelSpecification,
                              var slot : SlotModifier,
                              var componentIndex : Option[ComponentIndex] = None,
                              var offset : Option[ExpressionIndex] = None,
                              var dirAccess : Option[ExpressionIndex] = None) extends Access(name,componentIndex) {
  def prettyprint(out : PpStream) = {
    // FIXME: omit slot if numSlots of target field is 1
    out << name << '[' << slot << ']' << '@' << level
    if (offset.isDefined) out << "@" << offset
    if (componentIndex.isDefined) componentIndex.get.prettyprint(out)
    if (dirAccess.isDefined) out << ":" << dirAccess
  }

  def resolveField : knowledge.Field = {
    knowledge.StencilFieldCollection.getStencilFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get.field
  }

  def getBasicStencilFieldAccess : ir.StencilFieldAccess = {
    if (arrayIndex.isDefined || dirAccess.isDefined)
      Logger.warn(s"Discarding modifiers of access to stencilfield $name on level ${level.asInstanceOf[SingleLevelSpecification].level}")

    val stencilField = knowledge.StencilFieldCollection.getStencilFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get

    var numDims = stencilField.field.fieldLayout.numDimsGrid
    if (arrayIndex.isDefined) numDims += 1 // TODO: remove array index and update function after integration of vec types
    var multiIndex = ir.LoopOverDimensions.defIt(numDims)
    if (arrayIndex.isDefined)
      multiIndex(numDims - 1) = ir.IntegerConstant(arrayIndex.get)
    if (offset.isDefined) {
      var progressedOffset = offset.get.progressToIr
      while (progressedOffset.indices.length < numDims) progressedOffset.indices :+= ir.IntegerConstant(0)
      multiIndex += progressedOffset
    }

    ir.StencilFieldAccess(knowledge.StencilFieldSelection(stencilField, ir.IntegerConstant(stencilField.field.level), FieldAccess.resolveSlot(stencilField.field, slot), None), multiIndex)
  }

  def progressToIr : ir.Expression = {
    if (arrayIndex.isDefined && dirAccess.isDefined)
      Logger.warn(s"Access to stencilfield $name on level ${level.asInstanceOf[SingleLevelSpecification].level} has direction access and array subscript modifiers; array index will be given precendence, offset will be ignored")

    val stencilField = knowledge.StencilFieldCollection.getStencilFieldByIdentifier(name, level.asInstanceOf[SingleLevelSpecification].level).get

    var accessIndex = -1

    if (arrayIndex.isDefined)
      accessIndex = arrayIndex.get
    else if (dirAccess.isDefined)
      accessIndex = stencilField.stencil.findStencilEntryIndex(dirAccess.get.progressToIr).get

    var numDims = knowledge.Knowledge.dimensionality // TODO: resolve field info
    numDims += 1 // TODO: remove array index and update function after integration of vec types
    var multiIndex = ir.LoopOverDimensions.defIt(numDims)

    if (accessIndex < 0)
      multiIndex(numDims - 1) = ir.IntegerConstant(0)
    else
      multiIndex(numDims - 1) = ir.IntegerConstant(accessIndex)

    if (offset.isDefined) {
      var progressedOffset = offset.get.progressToIr
      while (progressedOffset.indices.length < numDims) progressedOffset.indices :+= ir.IntegerConstant(0)
      multiIndex += progressedOffset
    }

    if (accessIndex < 0)
      ir.StencilFieldAccess(knowledge.StencilFieldSelection(stencilField, ir.IntegerConstant(stencilField.field.level), FieldAccess.resolveSlot(stencilField.field, slot), None),
        multiIndex)
    else
      ir.FieldAccess(knowledge.FieldSelection(stencilField.field, ir.IntegerConstant(stencilField.field.level), FieldAccess.resolveSlot(stencilField.field, slot), Some(accessIndex)),
        multiIndex)
  }
}
