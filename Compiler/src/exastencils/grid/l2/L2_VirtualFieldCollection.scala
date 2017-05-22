package exastencils.grid.l2

import scala.collection.mutable.ListBuffer

import exastencils.grid.l3._
import exastencils.knowledge.l2.L2_KnowledgeStrategyContainer._
import exastencils.knowledge.l2._
import exastencils.logger.Logger

/// L2_VirtualFieldCollection

object L2_VirtualFieldCollection extends L2_LeveledKnowledgeCollection[L2_VirtualField, L3_VirtualField] {
  exastencils.core.Duplicate.registerConstant(this)

  L2_PrepareDeclarations.strategies += L2_PrepareVirtualFieldDeclarations
  L2_PrepareAccesses.strategies += L2_PrepareVirtualFieldAccesses
  L2_ProcessDeclarations.strategies += L2_ProcessVirtualFieldDeclarations
  L2_ResolveAccesses.strategies += L2_ResolveVirtualFieldAccesses

  def progress() = objects.foreach(obj => L3_VirtualFieldCollection.add(obj.progress()))

  // special overrides for handling name variations

  def prefixedLC(identifier : String) = (if (identifier.startsWith("vf_")) identifier else "vf_" + identifier).toLowerCase

  override def exists(identifier : String) = objects.exists(_.name.toLowerCase == prefixedLC(identifier))
  override def exists(identifier : String, level : Int) = objects.exists(f => f.name.toLowerCase == prefixedLC(identifier) && f.level == level)

  override def existsDecl(identifier : String) = declared.exists(_.name.toLowerCase == prefixedLC(identifier))
  override def existsDecl(identifier : String, level : Int) = declared.exists(f => f.name.toLowerCase == prefixedLC(identifier) && f.level == level)

  override def getByIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[L2_VirtualField] = {
    val ret = objects.find(f => f.name.toLowerCase == prefixedLC(identifier) && f.level == level)
    if (!suppressError && ret.isEmpty) Logger.warn(s"L2_VirtualField $identifier for level $level was not found")
    ret
  }

  override def getAllByIdentifier(identifier : String, suppressError : Boolean = false) : ListBuffer[L2_VirtualField] = {
    var foundObjs = ListBuffer[L2_VirtualField]()
    for (obj <- objects)
      if (obj.name.toLowerCase == prefixedLC(identifier))
        foundObjs += obj

    if (!suppressError && foundObjs.isEmpty) Logger.warn(s"L2_VirtualField $identifier was not found on any level")
    foundObjs
  }
}
