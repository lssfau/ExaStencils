package exastencils.fieldlike.l3

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._

import exastencils.base.ExaRootNode
import exastencils.base.l3._
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.l3._
import exastencils.fieldlike.l4.L4_FieldLike
import exastencils.knowledge.l3.L3_KnowledgeContainer._
import exastencils.knowledge.l3.L3_LeveledKnowledgeCollection
import exastencils.logger.Logger
import exastencils.util.l3.L3_LevelCollector
import exastencils.waLBerla.l3.L3_WaLBerlaFieldCollection
import exastencils.waLBerla.l3.L3_WaLBerlaFieldDecl

abstract class L3_FieldLikeCollection[L3_Type <: L3_FieldLike[L4_Type] : TypeTag, L4_Type <: L4_FieldLike[_, _]] extends L3_LeveledKnowledgeCollection[L3_Type, L4_Type] {

  L3_PrepareDeclarations.strategies += L3_PrepareFieldDeclarations
  L3_ProcessDeclarations.strategies += L3_ProcessFieldDeclarations

  L3_PrepareAccesses.strategies += L3_PrepareFieldAccesses
  L3_ResolveAccesses.strategies += L3_ResolveFieldAccesses

  // generate field layouts and add to L4 layout collection
  def prepareFieldLayouts() : Unit

  def addInitFieldsFunction() = {
    val initStmts = ListBuffer[L3_Statement]()
    for (field <- objects)
      if (field.initial.isDefined) // TODO: honor slots
        initStmts += L3_Assignment(L3_FieldAccess(field.toField), field.initial.get)
    val fct = L3_PlainFunction("InitFields", L3_UnitDatatype, ListBuffer(), initStmts)
    ExaRootNode.l3_root.nodes += fct
  }

  /// L3_PrepareFieldDeclarations

  // TODO: make strategy more generic
  object L3_PrepareFieldDeclarations extends DefaultStrategy("Prepare knowledge for L3 fields") {
    this += Transformation("Process new fields", {
      case decl : L3_FieldDecl if L3_FieldLikeCollection.this == L3_FieldCollection =>
        addDeclared(decl.name, decl.levels)
        decl // preserve declaration statement
      case decl : L3_WaLBerlaFieldDecl if L3_FieldLikeCollection.this == L3_WaLBerlaFieldCollection =>
        addDeclared(decl.name, decl.levels)
        decl // preserve declaration statement
    })
  }

  /// L3_ProcessFieldDeclarations

  object L3_ProcessFieldDeclarations extends DefaultStrategy("Integrate L3 field declarations with knowledge") {
    this += Transformation("Process field declarations", {
      case decl : L3_FieldLikeDecl[_] if existsDecl(decl.name, decl.levels) && L3_MayBlockResolution.isDone(decl) =>
        decl.addToKnowledge()
        None // consume declaration statement
    })
  }

  /// L3_PrepareFieldAccesses

  object L3_PrepareFieldAccesses extends DefaultStrategy("Prepare accesses to fields") {
    val collector = new L3_LevelCollector
    this.register(collector)
    this.onBefore = () => this.resetCollectors()

    this += new Transformation("Resolve applicable unresolved accesses", {
      case access : L3_UnresolvedAccess if existsDecl(access.name) =>
        val lvl = {
          if (access.level.isDefined) access.level.get.resolveLevel
          else if (collector.inLevelScope) collector.getCurrentLevel
          else Logger.error(s"Missing level for access to field ${ access.name }")
        }

        if (!existsDecl(access.name, lvl))
          Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

        if (access.dirAccess.isDefined) Logger.warn(s"Discarding meaningless direction access on ${ access.name }")
        if (access.arrayIndex.isDefined) Logger.warn(s"Discarding meaningless array access on ${ access.name }")

        L3_FutureFieldAccess(access.name, lvl, access.slot.getOrElse(L3_ActiveSlot), access.offset)
    })
  }

  /// L3_ResolveFieldAccesses

  object L3_ResolveFieldAccesses extends DefaultStrategy("Resolve accesses to fields") {
    this += new Transformation("Resolve applicable future accesses", {
      // check if declaration has already been processed and promote access if possible
      case access : L3_FutureFieldAccess if exists(access.name, access.level) =>
        val field = getByIdentifier(access.name, access.level).get
        L3_FieldAccess(field.toField, access.slot, access.offset, access.frozen)
    })
  }
}