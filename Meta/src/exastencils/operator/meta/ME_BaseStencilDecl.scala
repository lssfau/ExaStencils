package exastencils.operator.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_BaseStencilDecl extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/operator/|LAYER_LC|/|LAYER_UC|_BaseStencilDecl.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.operator.|LAYER_LC|

import scala.collection.mutable._

import exastencils.base.|LAYER_LC|._
import exastencils.prettyprinting._

/// |LAYER_UC|_BaseStencilDecl

object |LAYER_UC|_BaseStencilDecl {
  def apply(name : String, levels : Option[|LAYER_UC|_LevelSpecification], entries : List[|LAYER_UC|_StencilEntry]) =
    new |LAYER_UC|_BaseStencilDecl(name, levels, entries.to[ListBuffer])
}

case class |LAYER_UC|_BaseStencilDecl(
    var name : String,
    var levels : Option[|LAYER_UC|_LevelSpecification],
    var entries : ListBuffer[|LAYER_UC|_StencilEntry]) extends |LAYER_UC|_StencilDecl {

  override def prettyprint(out : PpStream) = out << "--- FIXME ---"

  override def addToKnowledge() : Unit = {
    // TODO: check stencil - numDims for entries, stride, etc.
    val numDims = entries.map(_.numDims).max
    val colStride = entries.map(_.colStride).head

    val level = |LAYER_UC|_LevelSpecification.asSingleLevel(levels)
    |LAYER_UC|_StencilCollection.add(|LAYER_UC|_Stencil(name, level, numDims, colStride, entries.map(_.asStencilMappingEntry)))
  }
}
"""
  }
}
