package exastencils.operator.l3

import scala.collection.mutable._

import exastencils.base.l3._
import exastencils.prettyprinting._

/// L3_BaseStencilDecl

object L3_BaseStencilDecl {
  def apply(name : String, levels : Option[L3_LevelSpecification], entries : List[L3_StencilEntry]) =
    new L3_BaseStencilDecl(name, levels, entries.to[ListBuffer])
}

case class L3_BaseStencilDecl(
    var name : String,
    var levels : Option[L3_LevelSpecification],
    var entries : ListBuffer[L3_StencilEntry]) extends L3_StencilDecl {

  override def prettyprint(out : PpStream) = out << "--- FIXME ---"

  override def addToKnowledge() : Unit = {
    // TODO: check stencil - numDims for entries, stride, etc.
    val numDims = entries.map(_.numDims).max
    val colStride = entries.map(_.colStride).head

    val level = L3_LevelSpecification.asSingleLevel(levels)
    L3_StencilCollection.add(L3_Stencil(name, level, numDims, colStride, entries.map(_.asStencilMappingEntry)))
  }
}
