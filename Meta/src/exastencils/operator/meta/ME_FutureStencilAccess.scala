package exastencils.operator.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_FutureStencilAccess extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/operator/|LAYER_LC|/|LAYER_UC|_FutureStencilAccess.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.operator.|LAYER_LC|

import exastencils.base.|LAYER_LC|.|LAYER_UC|_LevelCollector
import exastencils.baseExt.|LAYER_LC|.|LAYER_UC|_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.|LAYER_LC|.|LAYER_UC|_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// |LAYER_UC|_FutureStencilAccess

case class |LAYER_UC|_FutureStencilAccess(var name : String, var level : Int) extends |LAYER_UC|_FutureKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << name << '@' << level

  def progress = {
    Logger.warn(s"Trying to progress future stencil access to $name on level $level")
    ??? // TODO
  }

  def toStencilAccess = |LAYER_UC|_StencilAccess(this)
}

/// |LAYER_UC|_PrepareStencilAccesses

object |LAYER_UC|_PrepareStencilAccesses extends DefaultStrategy("Prepare accesses to stencils") {
  val collector = new |LAYER_UC|_LevelCollector
  this.register(collector)

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : |LAYER_UC|_UnresolvedAccess if |LAYER_UC|_StencilCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to stencil ${ access.name }")
      }

      if (!|LAYER_UC|_StencilCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      |LAYER_UC|_FutureStencilAccess(access.name, lvl)
  })
}

"""
  }
}
