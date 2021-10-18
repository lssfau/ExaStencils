package exastencils.waLBerla.l4

import exastencils.base.l4.L4_MayBlockResolution
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.l4.L4_ActiveSlot
import exastencils.field.l4.L4_FieldAccess
import exastencils.field.l4.L4_FutureFieldAccess
import exastencils.knowledge.l4.L4_KnowledgeContainer
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_PrepareAccesses
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_PrepareDeclarations
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_ProcessDeclarations
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_ResolveAccesses
import exastencils.knowledge.l4.L4_LeveledKnowledgeCollection
import exastencils.logger.Logger
import exastencils.util.l4.L4_LevelCollector
import exastencils.waLBerla.ir.IR_WaLBerlaField
import exastencils.waLBerla.ir.IR_WaLBerlaFieldCollection

object L4_WaLBerlaFieldCollection extends L4_LeveledKnowledgeCollection[L4_WaLBerlaField, IR_WaLBerlaField] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

  L4_PrepareDeclarations.strategies += L4_WaLBerlaPrepareFieldDeclarations
  L4_ProcessDeclarations.strategies += L4_WaLBerlaProcessFieldDeclarations

  L4_PrepareAccesses.strategies += L4_WaLBerlaPrepareFieldAccesses
  L4_ResolveAccesses.strategies += L4_WaLBerlaResolveFieldAccesses

  def getByFieldAccess(access : L4_FutureFieldAccess) : Option[L4_WaLBerlaField] = getByIdentifier(access.name, access.level, suppressError = true)
  def getByFieldAccess(access : L4_FieldAccess) : Option[L4_WaLBerlaField] = getByIdentifier(access.name, access.level, suppressError = true)

  def contains(access : L4_FutureFieldAccess) : Boolean = getByFieldAccess(access).isDefined
  def contains(access : L4_FieldAccess) : Boolean = getByFieldAccess(access).isDefined

  override def name = "L4_WaLBerlaFieldCollection"
  override def progress() : Unit = objects.foreach(obj => IR_WaLBerlaFieldCollection.add(obj.progress()))
}

/// L4_WaLBerlaPrepareFieldDeclarations

object L4_WaLBerlaPrepareFieldDeclarations extends DefaultStrategy("Prepare knowledge for waLBerla L4 fields") {
  this += Transformation("Process new fields", {
    case decl : L4_WaLBerlaFieldDecl =>
      L4_WaLBerlaFieldCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L4_WaLBerlaProcessFieldDeclarations

object L4_WaLBerlaProcessFieldDeclarations extends DefaultStrategy("Integrate L4 waLBerla field declarations with knowledge") {
  this += Transformation("Process field declarations", {
    case decl : L4_WaLBerlaFieldDecl if L4_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}

/// L4_WaLBerlaPrepareFieldAccesses

object L4_WaLBerlaPrepareFieldAccesses extends DefaultStrategy("Prepare accesses to waLBerla fields") {
  val collector = new L4_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L4_UnresolvedAccess if L4_WaLBerlaFieldCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to field ${ access.name }")
      }

      if (access.dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on field - was an offset access (@) intended?")

      if (!L4_WaLBerlaFieldCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      L4_FutureFieldAccess(access.name, lvl, access.slot.getOrElse(L4_ActiveSlot), access.offset, frozen = false, access.matIndex)
  })
}

/// L4_WaLBerlaResolveFieldAccesses

object L4_WaLBerlaResolveFieldAccesses extends DefaultStrategy("Resolve accesses to waLBerla fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case fAcc : L4_FutureFieldAccess if L4_WaLBerlaFieldCollection.exists(fAcc.name, fAcc.level) =>
      val wbField = L4_WaLBerlaFieldCollection.getByFieldAccess(fAcc).get // get field from wb field collection
      L4_FieldAccess(wbField.toField, fAcc.slot, fAcc.offset, fAcc.frozen, fAcc.matIndex) // create 'regular' access for it
  })
}
