package exastencils.fieldlike.l4

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._

import exastencils.base.l4.L4_MayBlockResolution
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.l4._
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.knowledge.l4.L4_KnowledgeContainer._
import exastencils.knowledge.l4.L4_LeveledKnowledgeCollection
import exastencils.logger.Logger
import exastencils.util.l4.L4_LevelCollector
import exastencils.waLBerla.l4.field._

object L4_FieldLikeCollections {
  val collections = ListBuffer[L4_FieldLikeCollection[_, _]]()

  def register(collection : L4_FieldLikeCollection[_, _]) = collections += collection

  def clear() = collections.foreach(_.clear())
}

abstract class L4_FieldLikeCollection[L4_Type <: L4_FieldLike[IR_Type, _] : TypeTag, IR_Type <: IR_FieldLike] extends L4_LeveledKnowledgeCollection[L4_Type, IR_Type] {

  L4_PrepareDeclarations.strategies += L4_PrepareFieldDeclarations
  L4_ProcessDeclarations.strategies += L4_ProcessFieldDeclarations

  L4_PrepareAccesses.strategies += L4_PrepareFieldAccesses
  L4_ResolveAccesses.strategies += L4_ResolveFieldAccesses

  def contains(access : L4_FutureFieldAccess) : Boolean = getByFieldAccess(access).isDefined
  def contains(access : L4_FieldAccess) : Boolean = getByFieldAccess(access).isDefined

  def getByFieldAccess(access : L4_FutureFieldAccess) : Option[L4_Type] = getByIdentifier(access.name, access.level, suppressError = true)
  def getByFieldAccess(access : L4_FieldAccess) : Option[L4_Type] = getByIdentifier(access.name, access.level, suppressError = true)

  // TODO: make strategies more generic

  /// L4_PrepareFieldDeclaration

  object L4_PrepareFieldDeclarations extends DefaultStrategy("Prepare knowledge for L4 fields") {
    this += Transformation("Process new fields", {
      case decl : L4_FieldDecl if L4_FieldLikeCollection.this == L4_FieldCollection =>
        addDeclared(decl.name, decl.levels)
        decl // preserve declaration statement
      case decl : L4_WaLBerlaFieldDecl if L4_FieldLikeCollection.this == L4_WaLBerlaFieldCollection =>
        addDeclared(decl.name, decl.levels)
        decl // preserve declaration statement
    })
  }

  /// L4_ProcessFieldDeclarations

  object L4_ProcessFieldDeclarations extends DefaultStrategy("Integrate L4 field declarations with knowledge") {
    this += Transformation("Process field declarations", {
      case decl : L4_FieldDecl if L4_FieldLikeCollection.this == L4_FieldCollection && L4_MayBlockResolution.isDone(decl) =>
        decl.addToKnowledge()
        None // consume declaration statement
      case decl : L4_WaLBerlaFieldDecl if L4_FieldLikeCollection.this == L4_WaLBerlaFieldCollection && L4_MayBlockResolution.isDone(decl) =>
        decl.addToKnowledge()
        None // consume declaration statement
    })
  }

  /// L4_PrepareFieldAccesses

  object L4_PrepareFieldAccesses extends DefaultStrategy("Prepare accesses to fields") {
    val collector = new L4_LevelCollector
    this.register(collector)
    this.onBefore = () => this.resetCollectors()

    this += new Transformation("Resolve applicable unresolved accesses", {
      case access : L4_UnresolvedAccess if existsDecl(access.name) =>
        val lvl = {
          if (access.level.isDefined) access.level.get.resolveLevel
          else if (collector.inLevelScope) collector.getCurrentLevel
          else Logger.error(s"Missing level for access to field ${ access.name }")
        }

        if (access.dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on field - was an offset access (@) intended?")

        if (!existsDecl(access.name, lvl))
          Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

        L4_FutureFieldAccess(access.name, lvl, access.slot.getOrElse(L4_ActiveSlot), access.offset, false, access.matIndex)
    })
  }

  /// L4_ResolveFieldAccesses

  object L4_ResolveFieldAccesses extends DefaultStrategy("Resolve accesses to fields") {
    this += new Transformation("Resolve applicable future accesses", {
      // check if declaration has already been processed and promote access if possible
      case access : L4_FutureFieldAccess if exists(access.name, access.level) =>
        val field = getByFieldAccess(access).get // get field from field collection
        L4_FieldAccess(field.toField, access.slot, access.offset, access.frozen, access.matIndex) // create 'regular' access for it
    })
  }
}
