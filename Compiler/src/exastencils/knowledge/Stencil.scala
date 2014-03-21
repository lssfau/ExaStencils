package exastencils.knowledge

import scala.collection.mutable.ListBuffer
import exastencils.core.collectors._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

case class StencilEntry(var offset : MultiIndex, var weight : Expression) {}

case class Stencil(var identifier : String, var entries : ListBuffer[StencilEntry] = new ListBuffer) extends Node {}

case class StencilCollection() extends Node {
  var stencils : ListBuffer[Stencil] = ListBuffer();

  def getStencilByIdentifier(identifier : String) : Option[Stencil] = {
    stencils.find(f => f.identifier == identifier)
  }
}

case class StencilConvolution(var stencil : Stencil, var field : Field, var targetIdx : MultiIndex = DefaultLoopMultiIndex()) extends Expression with Expandable {
  override def cpp : String = "NOT VALID ; CLASS = StencilConvolution\n";

  def expand(collector : StackCollector) : Expression = {
    stencil.entries.map(e =>
      e.weight.cpp * (new FieldAccess("curFragment.", field, 0, new MultiIndex(targetIdx, e.offset, _ + _))). /*FIXME*/ expand(new StackCollector))
      .toArray[Expression].reduceLeft(_ + _)
  }
}

