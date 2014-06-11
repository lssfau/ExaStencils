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

case class StencilConvolution(var stencil : Stencil, var field : Field, var targetIdx : MultiIndex = DefaultLoopMultiIndex()) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = StencilConvolution\n"

  def expand : Expression = {
    var ret : Expression = stencil.entries.map(e => e.weight * FieldAccess("curFragment.", field, 0, new MultiIndex(targetIdx, e.offset, _ + _)))
      .toArray[Expression].reduceLeft(_ + _)
    SimplifyStrategy.doUntilDoneStandalone(ret)
    ret
  }
}

case class StencilFieldConvolution(var stencilField : StencilField, var field : Field, var targetIdx : MultiIndex = DefaultLoopMultiIndex()) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = StencilConvolution\n"

  def resolveEntry(idx : Int) : Expression = {
    var stencilFieldIdx = Duplicate(targetIdx)
    stencilFieldIdx(Knowledge.dimensionality) = idx

    FieldAccess("curFragment.", stencilField.field, 0 /*FIXME*/ , stencilFieldIdx) *
      FieldAccess("curFragment.", field, 0 /*FIXME*/ , new MultiIndex(targetIdx, stencilField.stencil.entries(idx).offset, _ + _))
  }

  def expand : Expression = {
    var ret : Expression = (0 until stencilField.stencil.entries.size).toArray.map(idx => resolveEntry(idx)).toArray[Expression].reduceLeft(_ + _)
    SimplifyStrategy.doUntilDoneStandalone(ret)
    ret
  }
}

object FindStencilConvolutions extends DefaultStrategy("FindStencilConvolutions") {
  this += new Transformation("SearchAndMark", {
    case MultiplicationExpression(UnresolvedStencilAccess(stencilName, stencilLevel), UnresolvedFieldAccess(fieldOwner, fieldName, fieldLevel, fieldSlot, fieldIndex)) =>
      StencilConvolution(StencilCollection.getStencilByIdentifier(stencilName, stencilLevel).get,
        FieldCollection.getFieldByIdentifier(fieldName, fieldLevel).get, fieldIndex)
    case MultiplicationExpression(UnresolvedStencilFieldAccess(stencilFieldName, stencilFieldLevel), UnresolvedFieldAccess(fieldOwner, fieldName, fieldLevel, fieldSlot, fieldIndex)) =>
      StencilFieldConvolution(StencilFieldCollection.getStencilFieldByIdentifier(stencilFieldName, stencilFieldLevel).get,
        FieldCollection.getFieldByIdentifier(fieldName, fieldLevel).get, fieldIndex)
  })
}
