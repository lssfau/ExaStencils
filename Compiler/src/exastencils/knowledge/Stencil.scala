package exastencils.knowledge

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.core.collectors._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.strategies._

case class StencilEntry(var offset : MultiIndex, var weight : Expression) {}

case class Stencil(var identifier : String, var level : Int, var entries : ListBuffer[StencilEntry] = new ListBuffer) {}

object StencilCollection {
  var stencils : ListBuffer[Stencil] = ListBuffer()

  def getStencilByIdentifier(identifier : String, level : Int) : Option[Stencil] = {
    stencils.find(s => s.identifier == identifier && s.level == level)
  }
}

case class StencilField(var identifier : String, var field : Field, var stencil : Stencil) {}

object StencilFieldCollection {
  var stencilFields : ListBuffer[StencilField] = ListBuffer()

  def getStencilFieldByIdentifier(identifier : String, level : Int) : Option[StencilField] = {
    stencilFields.find(s => s.identifier == identifier && s.field.level == level)
  }
}

case class StencilFieldSelection(
    var stencilField : StencilField,
    var slot : Expression,
    var arrayIndex : Int,
    var fragIdx : Expression = "fragmentIdx") extends Node {

  def toFieldSelection = {
    new FieldSelection(field, slot, arrayIndex, fragIdx)
  }

  // shortcuts to stencilField members
  def field = stencilField.field
  def stencil = stencilField.stencil

  // shortcuts to Field members
  def codeName = field.codeName
  def dataType = field.dataType
  def layout = field.layout
  def level = field.level
  def referenceOffset = field.referenceOffset
}

case class StencilConvolution(var stencil : Stencil, var fieldAccess : FieldAccess) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = StencilConvolution\n"

  def resolveEntry(idx : Int) : Expression = {
    stencil.entries(idx).weight * new FieldAccess(fieldAccess.fieldSelection, fieldAccess.index + stencil.entries(idx).offset)
  }

  def expand : Expression = {
    var ret : Expression = (0 until stencil.entries.size).toArray.map(idx => resolveEntry(idx)).toArray[Expression].reduceLeft(_ + _)
    SimplifyStrategy.doUntilDoneStandalone(ret)
    ret
  }
}

case class StencilFieldConvolution(var stencilFieldAccess : StencilFieldAccess, var fieldAccess : FieldAccess) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = StencilConvolution\n"

  def resolveEntry(idx : Int) : Expression = {
    var stencilFieldIdx = Duplicate(stencilFieldAccess.index)
    stencilFieldIdx(Knowledge.dimensionality) = idx

    FieldAccess(stencilFieldAccess.stencilFieldSelection.toFieldSelection, stencilFieldIdx) *
      new FieldAccess(fieldAccess.fieldSelection, fieldAccess.index + stencilFieldAccess.stencilFieldSelection.stencil.entries(idx).offset)
  }

  def expand : Expression = {
    var ret : Expression = (0 until stencilFieldAccess.stencilFieldSelection.stencil.entries.size).toArray.map(idx => resolveEntry(idx)).toArray[Expression].reduceLeft(_ + _)
    SimplifyStrategy.doUntilDoneStandalone(ret)
    ret
  }
}

object FindStencilConvolutions extends DefaultStrategy("FindStencilConvolutions") {
  this += new Transformation("SearchAndMark", {
    case MultiplicationExpression(StencilAccess(stencil), fieldAccess : FieldAccess) =>
      StencilConvolution(stencil, fieldAccess)
    case MultiplicationExpression(stencilFieldAccess : StencilFieldAccess, fieldAccess : FieldAccess) =>
      StencilFieldConvolution(stencilFieldAccess, fieldAccess)
  })
}
