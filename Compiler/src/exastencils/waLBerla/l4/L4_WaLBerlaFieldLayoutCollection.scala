package exastencils.waLBerla.l4

import exastencils.base.l4.L4_MayBlockResolution
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.l4.L4_FutureFieldLayoutAccess
import exastencils.knowledge.l4.L4_KnowledgeAccess
import exastencils.knowledge.l4.L4_KnowledgeContainer
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_PrepareAccesses
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_PrepareDeclarations
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_ProcessDeclarations
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_ResolveAccesses
import exastencils.knowledge.l4.L4_LeveledKnowledgeCollection
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l4.L4_LevelCollector
import exastencils.waLBerla.ir.IR_WaLBerlaFieldLayout
import exastencils.waLBerla.ir.IR_WaLBerlaFieldLayoutCollection

object L4_WaLBerlaFieldLayoutCollection extends L4_LeveledKnowledgeCollection[L4_WaLBerlaFieldLayout, IR_WaLBerlaFieldLayout]  {

  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

  L4_PrepareDeclarations.strategies += L4_WaLBerlaPrepareFieldLayoutDeclarations
  L4_ProcessDeclarations.strategies += L4_WaLBerlaProcessFieldLayoutDeclarations

  L4_PrepareAccesses.strategies += L4_WaLBerlaPrepareFieldLayoutAccesses
  L4_ResolveAccesses.strategies += L4_WaLBerlaResolveFieldLayoutAccesses

  override def name = "L4_WaLBerlaFieldLayoutCollection"
  override def progress() = for (obj <- objects) IR_WaLBerlaFieldLayoutCollection.objects += obj.progress
}

/// L4_WaLBerlaFieldLayoutAccess

case class L4_WaLBerlaFieldLayoutAccess(var target : L4_WaLBerlaFieldLayout) extends L4_KnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name << "@" << target.level
  override def progress = Logger.error(s"Trying to progress access to waLBerla field layout ${ target.name }@${ target.level } - unsupported")
}

/// L4_WaLBerlaPrepareFieldLayoutDeclarations

object L4_WaLBerlaPrepareFieldLayoutDeclarations extends DefaultStrategy("Prepare knowledge for L4 waLBerla field layouts") {
  this += Transformation("Process new field layouts", {
    case decl : L4_WaLBerlaFieldLayoutDecl =>
      L4_WaLBerlaFieldLayoutCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L4_WaLBerlaProcessFieldLayoutDeclarations

object L4_WaLBerlaProcessFieldLayoutDeclarations extends DefaultStrategy("Integrate L4 waLBerla field layout declarations with knowledge") {
  this += Transformation("Process field layout declarations", {
    case decl : L4_WaLBerlaFieldLayoutDecl if L4_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}

/// L4_WaLBerlaPrepareFieldLayoutAccesses

object L4_WaLBerlaPrepareFieldLayoutAccesses extends DefaultStrategy("Prepare accesses to waLBerla field layouts") {
  val collector = new L4_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L4_UnresolvedAccess if L4_WaLBerlaFieldLayoutCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to field layout ${ access.name }")
      }

      if (access.offset.isDefined) Logger.warn("Discarding meaningless offset access on field layout")
      if (access.dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on field layout")

      if (!L4_WaLBerlaFieldLayoutCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      L4_FutureFieldLayoutAccess(access.name, lvl)
  })
}

/// L4_WaLBerlaResolveFieldLayoutAccesses

object L4_WaLBerlaResolveFieldLayoutAccesses extends DefaultStrategy("Resolve accesses to waLBerla field layouts") {
  this += new Transformation("Resolve applicable unresolved accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L4_FutureFieldLayoutAccess if L4_WaLBerlaFieldLayoutCollection.exists(access.name, access.level) =>
      val target = L4_WaLBerlaFieldLayoutCollection.getByIdentifier(access.name, access.level).get

      L4_WaLBerlaFieldLayoutAccess(target)
  })
}