package exastencils.grid.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_VirtualFieldCollection extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/grid/|LAYER_LC|/|LAYER_UC|_VirtualFieldCollection.scala"

  override def generateForLayer(layer : Layer) = {
    val printer = new Printer
    printer <<< """package exastencils.grid.|LAYER_LC|"""
    printer <<< """"""
    printer <<< """import scala.collection.mutable.ListBuffer"""
    printer <<< """"""
    printer <<< """import exastencils.grid.|NEXT_LC|._"""
    printer <<< """import exastencils.knowledge.|LAYER_LC|.|LAYER_UC|_KnowledgeContainer._"""
    printer <<< """import exastencils.knowledge.|LAYER_LC|._"""
    printer <<< """import exastencils.logger.Logger"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_VirtualFieldCollection"""
    printer <<< """"""
    printer <<< """object |LAYER_UC|_VirtualFieldCollection extends |LAYER_UC|_LeveledKnowledgeCollection[|LAYER_UC|_VirtualField, |NEXT_UC|_VirtualField] {"""
    printer <<< """  exastencils.core.Duplicate.registerConstant(this)"""
    printer <<< """"""
    printer <<< """  |LAYER_UC|_KnowledgeContainer.register(this)"""
    printer <<< """"""
    if (L2 == layer) {
      printer <<< """  |LAYER_UC|_PrepareDeclarations.strategies += |LAYER_UC|_PrepareVirtualFieldDeclarations"""
    }
    if (L3 == layer) {
      printer <<< """  if (false /* TODO: introduce sth like Knowledge.minLayer */ ) {"""
    }
    if (L2 == layer) {
      printer <<< """  |LAYER_UC|_ProcessDeclarations.strategies += |LAYER_UC|_ProcessVirtualFieldDeclarations"""
    }
    if (L3 == layer) {
      printer <<< """    |LAYER_UC|_PrepareDeclarations.strategies += |LAYER_UC|_PrepareVirtualFieldDeclarations"""
    }
    if (L3 == layer) {
      printer <<< """    |LAYER_UC|_ProcessDeclarations.strategies += |LAYER_UC|_ProcessVirtualFieldDeclarations"""
      printer <<< """  }"""
    }
    printer <<< """"""
    printer <<< """  |LAYER_UC|_PrepareAccesses.strategies += |LAYER_UC|_PrepareVirtualFieldAccesses"""
    printer <<< """  |LAYER_UC|_ResolveAccesses.strategies += |LAYER_UC|_ResolveVirtualFieldAccesses"""
    printer <<< """"""
    printer <<< """  override def name = "|LAYER_UC|_VirtualFieldCollection""""
    printer <<< """  override def progress() = objects.foreach(obj => |NEXT_UC|_VirtualFieldCollection.add(obj.progress()))"""
    printer <<< """"""
    printer <<< """  // special overrides for handling possible name variations"""
    printer <<< """"""
    printer <<< """  def prefixedLC(identifier : String) = (if (identifier.startsWith("vf_")) identifier else "vf_" + identifier).toLowerCase"""
    printer <<< """"""
    printer <<< """  override def exists(identifier : String) = objects.exists(_.name.toLowerCase == prefixedLC(identifier))"""
    printer <<< """  override def exists(identifier : String, level : Int) = objects.exists(f => f.name.toLowerCase == prefixedLC(identifier) && f.level == level)"""
    printer <<< """"""
    printer <<< """  override def existsDecl(identifier : String) = declared.exists(_.name.toLowerCase == prefixedLC(identifier))"""
    printer <<< """  override def existsDecl(identifier : String, level : Int) = declared.exists(f => f.name.toLowerCase == prefixedLC(identifier) && f.level == level)"""
    printer <<< """"""
    printer <<< """  override def getByIdentifier(identifier : String, level : Int, suppressError : Boolean = false) : Option[|LAYER_UC|_VirtualField] = {"""
    printer <<< """    val ret = objects.find(f => f.name.toLowerCase == prefixedLC(identifier) && f.level == level)"""
    printer <<< """    if (!suppressError && ret.isEmpty) Logger.warn(s"|LAYER_UC|_VirtualField $identifier for level $level was not found")"""
    printer <<< """    ret"""
    printer <<< """  }"""
    printer <<< """"""
    printer <<< """  override def getAllByIdentifier(identifier : String, suppressError : Boolean = false) : ListBuffer[|LAYER_UC|_VirtualField] = {"""
    printer <<< """    var foundObjs = ListBuffer[|LAYER_UC|_VirtualField]()"""
    printer <<< """    for (obj <- objects)"""
    printer <<< """      if (obj.name.toLowerCase == prefixedLC(identifier))"""
    printer <<< """        foundObjs += obj"""
    printer <<< """"""
    printer <<< """    if (!suppressError && foundObjs.isEmpty) Logger.warn(s"|LAYER_UC|_VirtualField $identifier was not found on any level")"""
    printer <<< """    foundObjs"""
    printer <<< """  }"""
    printer <<< """}"""
    printer.toString
  }
}
