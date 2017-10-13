package exastencils.grid.l3

import scala.collection.mutable.ListBuffer

import exastencils.grid.l4._
import exastencils.knowledge.l3.L3_KnowledgeContainer._
import exastencils.knowledge.l3._
import exastencils.logger.Logger

/// L3_VirtualFieldCollection

object L3_VirtualFieldCollection extends L3_LeveledKnowledgeCollection[L3_VirtualField, L4_VirtualField] {
  exastencils.core.Duplicate.registerConstant(this)

  L3_KnowledgeContainer.register(this)

  L3_PrepareDeclarations.strategies += L3_PrepareVirtualFieldDeclarations
  L3_ProcessDeclarations.strategies += L3_ProcessVirtualFieldDeclarations

  L3_PrepareAccesses.strategies += L3_PrepareVirtualFieldAccesses
  L3_ResolveAccesses.strategies += L3_ResolveVirtualFieldAccesses

  override def name = "L3_VirtualFieldCollection"
  override def progress() = objects.foreach(obj => L4_VirtualFieldCollection.add(obj.progress()))

  // special overrides for handling possible name variations

  def prefixedLC(identifier : String) = (if (identifier.startsWith("vf_")) identifier else "vf_" + identifier).toLowerCase

  override def exists(identifier : String) = objects.exists(_.name.toLowerCase == prefixedLC(identifier))
  override def exists(identifier : String, level : Int) = objects.exists(f => f.name.toLowerCase == prefixedLC(identifier) && f.level == level)

  override def existsDecl(identifier : String) = declared.exists(_.name.toLowerCase == prefixedLC(identifier))
  override def existsDecl(identifier : String, level : Int) = declared.exists(f => f.name.toLowerCase == prefixedLC(identifier) && f.level == level)

  override def getByIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[L3_VirtualField] = {
    val ret = objects.find(f => f.name.toLowerCase == prefixedLC(identifier) && f.level == level)
    if (!suppressError && ret.isEmpty) Logger.warn(s"L3_VirtualField $identifier for level $level was not found")
    ret
  }

  override def getAllByIdentifier(identifier : String, suppressError : Boolean = false) : ListBuffer[L3_VirtualField] = {
    var foundObjs = ListBuffer[L3_VirtualField]()
    for (obj <- objects)
      if (obj.name.toLowerCase == prefixedLC(identifier))
        foundObjs += obj

    if (!suppressError && foundObjs.isEmpty) Logger.warn(s"L3_VirtualField $identifier was not found on any level")
    foundObjs
  }
}
