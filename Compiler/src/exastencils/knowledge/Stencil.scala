package exastencils.knowledge

import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.core.collectors._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.strategies._

case class StencilEntry(var offset : MultiIndex, var weight : Expression) extends Node {}

case class Stencil(var identifier : String, var level : Int, var entries : ListBuffer[StencilEntry] = new ListBuffer) extends Node {}

case class StencilCollection() extends Node {
  var stencils : ListBuffer[Stencil] = ListBuffer();

  def getStencilByIdentifier(identifier : String, level : Int) : Option[Stencil] = {
    stencils.find(s => s.identifier == identifier && s.level == level)
  }
}

case class StencilConvolution(var stencil : Stencil, var field : Field, var targetIdx : MultiIndex = DefaultLoopMultiIndex()) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = StencilConvolution\n";

  def expand : Expression = {
    var ret : Expression = stencil.entries.map(e => e.weight * FieldAccess("curFragment.", field, 0, new MultiIndex(targetIdx, e.offset, _ + _)))
      .toArray[Expression].reduceLeft(_ + _)
    SimplifyStrategy.doUntilDoneStandalone(ret)
    ret
  }
}

object FindStencilConvolutions extends DefaultStrategy("FindStencilConvolutions") {
  this += new Transformation("SearchAndMark", {
    case MultiplicationExpression(UnresolvedStencilAccess(stencilName, stencilLevel), UnresolvedFieldAccess(fieldOwner, fieldName, fieldLevel, fieldSlot, fieldIndex)) =>
      StencilConvolution(StateManager.findFirst[StencilCollection]().get.getStencilByIdentifier(stencilName, stencilLevel).get,
        StateManager.findFirst[FieldCollection]().get.getFieldByIdentifier(fieldName, fieldLevel).get)
  })
}
