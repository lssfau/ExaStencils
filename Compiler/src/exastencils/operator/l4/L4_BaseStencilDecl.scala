package exastencils.operator.l4

import scala.collection.mutable._

import exastencils.base.l4._
import exastencils.prettyprinting._

/// L4_BaseStencilDecl

object L4_BaseStencilDecl {
  def apply(name : String, levels : Option[L4_LevelSpecification], entries : List[L4_StencilEntry]) =
    new L4_BaseStencilDecl(name, levels, entries.to[ListBuffer])
}

case class L4_BaseStencilDecl(
    var name : String,
    var levels : Option[L4_LevelSpecification],
    var entries : ListBuffer[L4_StencilEntry]) extends L4_StencilDecl {

  override def prettyprint(out : PpStream) = {
    out << "Stencil " << name
    if (levels.isDefined) out << "@" << levels.get
    out << "{\n" <<< (entries, "\n") << "\n}"
  }

  override def addToKnowledge() : Unit = {
    // TODO: check stencil - numDims for entries, stride, etc.
    val numDims = entries.map(_.numDims).max
    val colStride = entries.map(_.colStride).head

    val level = L4_LevelSpecification.asSingleLevel(levels)
    L4_StencilCollection.add(L4_Stencil(name, level, numDims, colStride, entries.map(_.asStencilMappingEntry)))
  }
}
