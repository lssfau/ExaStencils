package exastencils.fieldlike.l2

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._

import exastencils.base.l2.L2_MayBlockResolution
import exastencils.baseExt.l2.L2_UnresolvedAccess
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.l2._
import exastencils.fieldlike.l3.L3_FieldLike
import exastencils.knowledge.l2.L2_KnowledgeContainer._
import exastencils.knowledge.l2.L2_LeveledKnowledgeCollection
import exastencils.logger.Logger
import exastencils.util.l2.L2_LevelCollector

object L2_FieldLikeCollections {
  val collections = ListBuffer[L2_FieldLikeCollection[_ <: L2_FieldLike[_], _ <: L3_FieldLike[_]]]()

  def register(collection : L2_FieldLikeCollection[_ <: L2_FieldLike[_], _ <: L3_FieldLike[_]]) =
    collections += collection

  def getByIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[L2_FieldLike[_]] = {
    for (coll <- collections) {
      if (coll.exists(identifier, level))
        return Some(coll.getByIdentifier(identifier, level, suppressError).get)
    }
    None
  }

  def clear() = collections.foreach(_.clear())
}

abstract class L2_FieldLikeCollection[L2_Type <: L2_FieldLike[L3_Type] : TypeTag, L3_Type <: L3_FieldLike[_]] extends L2_LeveledKnowledgeCollection[L2_Type, L3_Type] {

  L2_PrepareDeclarations.strategies += L2_PrepareFieldLikeDeclarations
  L2_ProcessDeclarations.strategies += L2_ProcessFieldLikeDeclarations

  L2_PrepareAccesses.strategies += L2_PrepareFieldLikeAccesses
  L2_ResolveAccesses.strategies += L2_ResolveFieldLikeAccesses

  /// L2_PrepareFieldDeclaration

  object L2_PrepareFieldLikeDeclarations extends DefaultStrategy("Prepare knowledge for L2 fields") {
    this += Transformation("Process new fields", {
      case decl : L2_FieldLikeDecl[_, _] if L2_FieldLikeCollection.this == decl.associatedCollection =>
        addDeclared(decl.name, decl.levels)
        decl // preserve declaration statement
    })
  }

  /// L2_ProcessFieldDeclarations

  object L2_ProcessFieldLikeDeclarations extends DefaultStrategy("Integrate L2 field declarations with knowledge") {
    this += Transformation("Process field declarations", {
      case decl : L2_FieldLikeDecl[_, _] if L2_FieldLikeCollection.this == decl.associatedCollection && L2_MayBlockResolution.isDone(decl) =>
        decl.addToKnowledge()
        None // consume declaration statement
    })
  }

  /// L2_PrepareFieldAccesses

  object L2_PrepareFieldLikeAccesses extends DefaultStrategy("Prepare accesses to fields") {
    val collector = new L2_LevelCollector
    this.register(collector)
    this.onBefore = () => this.resetCollectors()

    this += new Transformation("Resolve applicable unresolved accesses", {
      case access : L2_UnresolvedAccess if existsDecl(access.name) =>
        val lvl = {
          if (access.level.isDefined) access.level.get.resolveLevel
          else if (collector.inLevelScope) collector.getCurrentLevel
          else Logger.error(s"Missing level for access to field ${ access.name }")
        }

        if (!existsDecl(access.name, lvl))
          Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

        if (access.slot.isDefined) Logger.warn(s"Discarding meaningless slot access on ${ access.name }")
        if (access.dirAccess.isDefined) Logger.warn(s"Discarding meaningless direction access on ${ access.name }")
        if (access.arrayIndex.isDefined) Logger.warn(s"Discarding meaningless array access on ${ access.name }")

        L2_FutureFieldAccess(access.name, lvl, access.offset)
    })
  }

  /// L2_ResolveFieldAccesses

  object L2_ResolveFieldLikeAccesses extends DefaultStrategy("Resolve accesses to fields") {
    this += new Transformation("Resolve applicable future accesses", {
      // check if declaration has already been processed and promote access if possible
      case access : L2_FutureFieldAccess if exists(access.name, access.level) =>
        val field = getByIdentifier(access.name, access.level).get
        L2_FieldAccess(field.toField, access.offset, access.frozen)
    })
  }
}

