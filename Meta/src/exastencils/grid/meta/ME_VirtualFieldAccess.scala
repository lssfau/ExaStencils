package exastencils.grid.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_VirtualFieldAccess extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/grid/|LAYER_LC|/|LAYER_UC|_VirtualFieldAccess.scala"

  override def generateForLayer(layer : Layer) = {
    val printer = new Printer
    printer <<< """package exastencils.grid.|LAYER_LC|"""
    printer <<< """"""
    if (L3 == layer) {
      printer <<< """import exastencils.base.|NEXT_LC|.|NEXT_UC|_SingleLevel"""
    }
    printer <<< """import exastencils.datastructures._"""
    if (L3 == layer) {
      printer <<< """import exastencils.grid.|NEXT_LC|.|NEXT_UC|_VirtualFieldAccess"""
    }
    if (L2 == layer) {
      printer <<< """import exastencils.grid.|NEXT_LC|._"""
    }
    printer <<< """import exastencils.knowledge.|LAYER_LC|.|LAYER_UC|_LeveledKnowledgeAccess"""
    printer <<< """import exastencils.prettyprinting.PpStream"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_VirtualFieldAccess"""
    printer <<< """"""
    printer <<< """object |LAYER_UC|_VirtualFieldAccess {"""
    printer <<< """  def apply(access : |LAYER_UC|_FutureVirtualFieldAccess) ="""
    printer <<< """    new |LAYER_UC|_VirtualFieldAccess(|LAYER_UC|_VirtualFieldCollection.getByIdentifier(access.name, access.level).get)"""
    printer <<< """}"""
    printer <<< """"""
    printer <<< """case class |LAYER_UC|_VirtualFieldAccess(var target : |LAYER_UC|_VirtualField) extends |LAYER_UC|_LeveledKnowledgeAccess {"""
    printer <<< """  def prettyprint(out : PpStream) = out << target.name << '@' << target.level"""
    if (L2 == layer) {
      printer <<< """  def progress = |NEXT_UC|_VirtualFieldAccess(target.getProgressedObj())"""
    }
    if (L3 == layer) {
      printer <<< """  def progress = |NEXT_UC|_VirtualFieldAccess(target.name, |NEXT_UC|_SingleLevel(target.level))"""
    }
    printer <<< """}"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_ResolveVirtualFieldAccesses"""
    printer <<< """"""
    printer <<< """object |LAYER_UC|_ResolveVirtualFieldAccesses extends DefaultStrategy("Resolve accesses to virtual fields") {"""
    printer <<< """  this += new Transformation("Resolve applicable future accesses", {"""
    printer <<< """    // check if declaration has already been processed and promote access if possible"""
    printer <<< """    case access : |LAYER_UC|_FutureVirtualFieldAccess if |LAYER_UC|_VirtualFieldCollection.exists(access.name, access.level) =>"""
    printer <<< """      access.toVirtualFieldAccess"""
    printer <<< """  })"""
    printer <<< """}"""
    printer.toString
  }
}
