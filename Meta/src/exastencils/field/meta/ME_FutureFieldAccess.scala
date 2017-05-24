package exastencils.field.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_FutureFieldAccess extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/field/|LAYER_LC|/|LAYER_UC|_FutureFieldAccess.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.field.|LAYER_LC|

import exastencils.base.|LAYER_LC|.|LAYER_UC|_LevelCollector
import exastencils.baseExt.|LAYER_LC|.|LAYER_UC|_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.|LAYER_LC|.|LAYER_UC|_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// |LAYER_UC|_FutureFieldAccess

case class |LAYER_UC|_FutureFieldAccess(var name : String, var level : Int) extends |LAYER_UC|_FutureKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << name << '@' << level

  def progress = {
    Logger.warn(s"Trying to progress future field access to $name on level $level")
    ??? // TODO
  }

  def toFieldAccess = |LAYER_UC|_FieldAccess(this)
}

/// |LAYER_UC|_PrepareFieldAccesses

object |LAYER_UC|_PrepareFieldAccesses extends DefaultStrategy("Prepare accesses to fields") {
  val collector = new |LAYER_UC|_LevelCollector
  this.register(collector)

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : |LAYER_UC|_UnresolvedAccess if |LAYER_UC|_FieldCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to field ${ access.name }")
      }

      if (!|LAYER_UC|_FieldCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      |LAYER_UC|_FutureFieldAccess(access.name, lvl)
  })
}

"""
  }
}
