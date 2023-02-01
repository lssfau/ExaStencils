package exastencils.fieldlike.l4

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.l4._
import exastencils.fieldlike.ir.IR_FieldLikeLayout
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_PrepareAccesses
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_PrepareDeclarations
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_ProcessDeclarations
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_ResolveAccesses
import exastencils.knowledge.l4.L4_LeveledKnowledgeCollection
import exastencils.logger.Logger
import exastencils.util.l4.L4_LevelCollector
import exastencils.waLBerla.l4.field._

object L4_FieldLikeLayoutCollections {
  val collections = ListBuffer[L4_FieldLikeLayoutCollection[_ <: L4_FieldLikeLayout[_], _ <: IR_FieldLikeLayout]]()

  def register(collection : L4_FieldLikeLayoutCollection[_ <: L4_FieldLikeLayout[_], _ <: IR_FieldLikeLayout]) =
    collections += collection

  def getByIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[L4_FieldLikeLayout[_]] = {
    for (coll <- collections) {
      if (coll.exists(identifier, level))
        return Some(coll.getByIdentifier(identifier, level, suppressError).get)
    }
    None
  }

  def clear() = collections.foreach(_.clear())
}

abstract class L4_FieldLikeLayoutCollection[L4_Type <: L4_FieldLikeLayout[IR_Type] : TypeTag, IR_Type <: IR_FieldLikeLayout] extends L4_LeveledKnowledgeCollection[L4_Type, IR_Type] {

  L4_PrepareDeclarations.strategies += L4_PrepareFieldLayoutDeclarations
  L4_ProcessDeclarations.strategies += L4_ProcessFieldLayoutDeclarations

  L4_PrepareAccesses.strategies += L4_PrepareFieldLayoutAccesses
  L4_ResolveAccesses.strategies += L4_ResolveFieldLayoutAccesses

  // TODO: make strategies more generic

  /// L4_PrepareFieldLayoutDeclaration

  object L4_PrepareFieldLayoutDeclarations extends DefaultStrategy("Prepare knowledge for L4 field layouts") {
    this += Transformation("Process new field layouts", {
      case decl : L4_FieldLayoutDecl if L4_FieldLikeLayoutCollection.this == L4_FieldLayoutCollection                 =>
        addDeclared(decl.name, decl.levels)
        decl // preserve declaration statement
      case decl : L4_WaLBerlaFieldLayoutDecl if L4_FieldLikeLayoutCollection.this == L4_WaLBerlaFieldLayoutCollection =>
        addDeclared(decl.name, decl.levels)
        decl // preserve declaration statement
    })
  }

  /// L4_ProcessFieldLayoutDeclarations

  object L4_ProcessFieldLayoutDeclarations extends DefaultStrategy("Integrate L4 field layout declarations with knowledge") {
    this += Transformation("Process field layout declarations", {
      case decl : L4_FieldLayoutDecl if L4_FieldLikeLayoutCollection.this == L4_FieldLayoutCollection && L4_MayBlockResolution.isDone(decl)                 =>
        decl.addToKnowledge()
        None // consume declaration statement
      case decl : L4_WaLBerlaFieldLayoutDecl if L4_FieldLikeLayoutCollection.this == L4_WaLBerlaFieldLayoutCollection && L4_MayBlockResolution.isDone(decl) =>
        decl.addToKnowledge()
        None // consume declaration statement
    })
  }

  /// L4_PrepareFieldLayoutAccesses

  object L4_PrepareFieldLayoutAccesses extends DefaultStrategy("Prepare accesses to field layouts") {
    val collector = new L4_LevelCollector
    this.register(collector)
    this.onBefore = () => this.resetCollectors()

    this += new Transformation("Resolve applicable unresolved accesses", {
      case access : L4_UnresolvedAccess if existsDecl(access.name) =>
        val lvl = {
          if (access.level.isDefined) access.level.get.resolveLevel
          else if (collector.inLevelScope) collector.getCurrentLevel
          else Logger.error(s"Missing level for access to field layout ${ access.name }")
        }

        if (access.offset.isDefined) Logger.warn("Discarding meaningless offset access on field layout")
        if (access.dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on field layout")

        if (!existsDecl(access.name, lvl))
          Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

        L4_FutureFieldLayoutAccess(access.name, lvl)
    })
  }

  /// L4_ResolveFieldLayoutAccesses

  object L4_ResolveFieldLayoutAccesses extends DefaultStrategy("Resolve accesses to field layouts") {
    this += new Transformation("Resolve applicable unresolved accesses", {
      // check if declaration has already been processed and promote access if possible
      case access : L4_FutureFieldLayoutAccess if exists(access.name, access.level) =>
        val layout = getByIdentifier(access.name, access.level).get
        layout.toLayoutAccess
    })
  }
}
