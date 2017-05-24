package exastencils.operator.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_StencilAccess extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/operator/|LAYER_LC|/|LAYER_UC|_StencilAccess.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.operator.|LAYER_LC|

import exastencils.datastructures._
import exastencils.knowledge.|LAYER_LC|._
import exastencils.operator.|NEXT_LC|.|NEXT_UC|_StencilAccess
import exastencils.prettyprinting.PpStream

/// |LAYER_UC|_StencilAccess

object |LAYER_UC|_StencilAccess {
  def apply(name : String, level : Int) =
    new |LAYER_UC|_StencilAccess(|LAYER_UC|_StencilCollection.getByIdentifier(name, level).get)

  def apply(access : |LAYER_UC|_FutureStencilAccess) =
    new |LAYER_UC|_StencilAccess(|LAYER_UC|_StencilCollection.getByIdentifier(access.name, access.level).get)
}

case class |LAYER_UC|_StencilAccess(var target : |LAYER_UC|_Stencil) extends |LAYER_UC|_LeveledKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name << '@' << target.level
  def progress = |NEXT_UC|_StencilAccess(target.getProgressedObj())
}

/// |LAYER_UC|_ResolveStencilAccesses

object |LAYER_UC|_ResolveStencilAccesses extends DefaultStrategy("Resolve accesses to stencils") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : |LAYER_UC|_FutureStencilAccess if |LAYER_UC|_StencilCollection.exists(access.name, access.level) =>
      access.toStencilAccess
  })
}
"""
  }
}
