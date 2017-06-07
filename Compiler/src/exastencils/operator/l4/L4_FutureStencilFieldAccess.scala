package exastencils.operator.l4

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.field.l4._
import exastencils.knowledge.l4.L4_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_FutureStencilFieldAccess

case class L4_FutureStencilFieldAccess(
    var name : String,
    var level : Int,
    var slot : L4_SlotSpecification,
    var offset : Option[L4_ConstIndex] = None,
    var dirAccess : Option[L4_ConstIndex] = None,
    var arrayIndex : Option[Int] = None) extends L4_FutureKnowledgeAccess {

  override def prettyprint(out : PpStream) = out << name << '@' << level

  def progress = {
    Logger.warn(s"Trying to progress future stencil field access to $name on level $level")
    ??? // TODO
  }

  def toStencilFieldAccess = L4_StencilFieldAccess(this)
}

/// L4_PrepareStencilFieldAccesses

object L4_PrepareStencilFieldAccesses extends DefaultStrategy("Prepare accesses to stencil fields") {
  val collector = new L4_LevelCollector
  this.register(collector)

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L4_UnresolvedAccess if L4_StencilFieldCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to stencil field ${ access.name }")
      }

      if (!L4_StencilFieldCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      L4_FutureStencilFieldAccess(access.name, lvl, access.slot.getOrElse(L4_ActiveSlot), access.offset, access.dirAccess, access.arrayIndex)
  })
}
