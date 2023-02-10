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

object L4_FieldLikeCollections {
  val collections = ListBuffer[L4_FieldLikeCollection[_ <: L4_FieldLike[_, _], _ <: IR_FieldLike]]()

  def register(collection : L4_FieldLikeCollection[_ <: L4_FieldLike[_, _], _ <: IR_FieldLike]) =
    collections += collection

  def getByIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[L4_FieldLike[_, _]] = {
    for (coll <- collections) {
      if (coll.exists(identifier, level))
        return Some(coll.getByIdentifier(identifier, level, suppressError).get)
    }
    None
  }

  def clear() = collections.foreach(_.clear())
}

abstract class L4_FieldLikeCollection[L4_Type <: L4_FieldLike[IR_Type, _] : TypeTag, IR_Type <: IR_FieldLike] extends L4_LeveledKnowledgeCollection[L4_Type, IR_Type] {

  L4_PrepareDeclarations.strategies += L4_PrepareFieldLikeDeclarations
  L4_ProcessDeclarations.strategies += L4_ProcessFieldLikeDeclarations

  L4_PrepareAccesses.strategies += L4_PrepareFieldLikeAccesses
  L4_ResolveAccesses.strategies += L4_ResolveFieldLikeAccesses

  def contains(access : L4_FutureFieldAccess) : Boolean = getByFieldAccess(access).isDefined
  def contains(access : L4_FieldLikeAccess) : Boolean = getByFieldAccess(access).isDefined

  def getByFieldAccess(access : L4_FutureFieldAccess) : Option[L4_Type] = getByIdentifier(access.name, access.level, suppressError = true)
  def getByFieldAccess(access : L4_FieldLikeAccess) : Option[L4_Type] = getByIdentifier(access.name, access.level, suppressError = true)

  /// L4_PrepareFieldDeclaration

  object L4_PrepareFieldLikeDeclarations extends DefaultStrategy("Prepare knowledge for L4 fields") {
    this += Transformation("Process new fields", {
      case decl : L4_FieldLikeDecl[_, _] if L4_FieldLikeCollection.this == decl.associatedCollection =>
        addDeclared(decl.name, decl.levels)
        decl // preserve declaration statement
    })
  }

  /// L4_ProcessFieldDeclarations

  object L4_ProcessFieldLikeDeclarations extends DefaultStrategy("Integrate L4 field declarations with knowledge") {
    this += Transformation("Process field declarations", {
      case decl : L4_FieldLikeDecl[_, _] if L4_FieldLikeCollection.this == decl.associatedCollection && L4_MayBlockResolution.isDone(decl) =>
        decl.addToKnowledge()
        None // consume declaration statement
    })
  }

  /// L4_PrepareFieldAccesses

  object L4_PrepareFieldLikeAccesses extends DefaultStrategy("Prepare accesses to fields") {
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

  object L4_ResolveFieldLikeAccesses extends DefaultStrategy("Resolve accesses to fields") {
    this += new Transformation("Resolve applicable future accesses", {
      // check if declaration has already been processed and promote access if possible
      case access : L4_FutureFieldAccess if exists(access.name, access.level) =>
        val field = getByFieldAccess(access).get // get field from field collection
        L4_FieldLikeAccess(field, access.slot, access.offset, access.frozen, access.matIndex)
    })
  }
}
