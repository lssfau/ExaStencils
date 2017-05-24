package exastencils.field.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_FieldAccess extends Generatable {
  override def validLayers() = ListBuffer(L2, L3)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/field/|LAYER_LC|/|LAYER_UC|_FieldAccess.scala"

  override def generateForLayer(layer : Layer) = {
    val printer = new Printer
    printer <<< """package exastencils.field.|LAYER_LC|"""
    printer <<< """"""
    printer <<< """import exastencils.datastructures._"""
    if (L2 == layer) {
      printer <<< """import exastencils.field.|NEXT_LC|.|NEXT_UC|_FieldAccess"""
    }
    if (L3 == layer) {
      printer <<< """import exastencils.field.|NEXT_LC|._"""
    }
    printer <<< """import exastencils.knowledge.|LAYER_LC|._"""
    printer <<< """import exastencils.prettyprinting.PpStream"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_FieldAccess"""
    printer <<< """"""
    printer <<< """object |LAYER_UC|_FieldAccess {"""
    printer <<< """  def apply(name : String, level : Int) ="""
    printer <<< """    new |LAYER_UC|_FieldAccess(|LAYER_UC|_FieldCollection.getByIdentifier(name, level).get)"""
    printer <<< """"""
    printer <<< """  def apply(access : |LAYER_UC|_FutureFieldAccess) ="""
    printer <<< """    new |LAYER_UC|_FieldAccess(|LAYER_UC|_FieldCollection.getByIdentifier(access.name, access.level).get)"""
    printer <<< """}"""
    printer <<< """"""
    printer <<< """case class |LAYER_UC|_FieldAccess(var target : |LAYER_UC|_Field) extends |LAYER_UC|_LeveledKnowledgeAccess {"""
    printer <<< """  override def prettyprint(out : PpStream) = out << target.name << '@' << target.level"""
    if (L3 == layer) {
      printer <<< """  override def progress = |NEXT_UC|_FieldAccess(target.getProgressedObj(), |NEXT_UC|_ActiveSlot)"""
    }
    if (L2 == layer) {
      printer <<< """  override def progress = |NEXT_UC|_FieldAccess(target.getProgressedObj())"""
    }
    printer <<< """}"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_ResolveFieldAccesses"""
    printer <<< """"""
    printer <<< """object |LAYER_UC|_ResolveFieldAccesses extends DefaultStrategy("Resolve accesses to fields") {"""
    printer <<< """  this += new Transformation("Resolve applicable future accesses", {"""
    printer <<< """    // check if declaration has already been processed and promote access if possible"""
    printer <<< """    case access : |LAYER_UC|_FutureFieldAccess if |LAYER_UC|_FieldCollection.exists(access.name, access.level) =>"""
    printer <<< """      access.toFieldAccess"""
    printer <<< """  })"""
    printer <<< """}"""
    printer.toString
  }
}
