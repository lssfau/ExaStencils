package exastencils.operator.l2

import scala.collection.mutable._

import exastencils.base.l2._
import exastencils.prettyprinting._

/// L2_BaseStencilDecl

object L2_BaseStencilDecl {
  def apply(name : String, levels : Option[L2_LevelSpecification], entries : List[L2_StencilEntry]) =
    new L2_BaseStencilDecl(name, levels, entries.to[ListBuffer])
}

case class L2_BaseStencilDecl(
    var name : String,
    var levels : Option[L2_LevelSpecification],
    var entries : ListBuffer[L2_StencilEntry]) extends L2_StencilDecl {

  override def prettyprint(out : PpStream) = {
    out << "Operator " << name
    if (levels.isDefined) out << '@' << levels.get
    out << " from Stencil {\n" <<< (entries, "\n") << "\n}"
  }

  override def addToKnowledge() : Unit = {
    // TODO: check stencil - numDims for entries, stride, etc.
    val numDims = entries.map(_.numDims).max
    val colStride = entries.map(_.colStride).head

    val level = L2_LevelSpecification.asSingleLevel(levels)
    L2_StencilCollection.add(L2_Stencil(name, level, numDims, colStride, entries.map(_.asStencilMappingEntry)))
  }
}
