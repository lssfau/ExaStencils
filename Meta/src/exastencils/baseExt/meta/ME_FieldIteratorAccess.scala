package exastencils.baseExt.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_FieldIteratorAccess extends Generatable {
  override def validLayers() = ListBuffer(L2, L3, L4, IR)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/baseExt/|LAYER_LC|/|LAYER_UC|_FieldIteratorAccess.scala"

  override def generateForLayer(layer : Layer) = {
    val printer = new Printer
    printer <<< """package exastencils.baseExt.|LAYER_LC|"""
    printer <<< """"""
    printer <<< """import exastencils.base.|LAYER_LC|._"""
    if (L2 == layer || L3 == layer || L4 == layer) {
      printer <<< """import exastencils.baseExt.|NEXT_LC|.|NEXT_UC|_FieldIteratorAccess"""
    }
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_FieldIteratorAccess"""
    printer <<< """"""
    printer <<< """object |LAYER_UC|_FieldIteratorAccess {"""
    printer <<< """  def apply(dim : Int) = {"""
    printer <<< """    val ret = new |LAYER_UC|_FieldIteratorAccess()"""
    printer <<< """    ret.dim = dim"""
    printer <<< """    ret"""
    printer <<< """  }"""
    printer <<< """}"""
    printer <<< """"""
    printer <<< """class |LAYER_UC|_FieldIteratorAccess() extends |LAYER_UC|_VariableAccess("i", |LAYER_UC|_IntegerDatatype) {"""
    printer <<< """  private var dim_ : Int = 0"""
    printer <<< """  def dim_=(d : Int) = {"""
    printer <<< """    dim_ = d"""
    printer <<< """    name = s"i$dim_""""
    printer <<< """  }"""
    printer <<< """  def dim = dim_"""
    printer <<< """"""
    if (L2 == layer || L3 == layer || L4 == layer) {
      printer <<< """  override def progress = |NEXT_UC|_FieldIteratorAccess(dim)"""
      printer <<< """"""
    }
    printer <<< """  override def equals(obj : scala.Any) = {"""
    printer <<< """    obj match {"""
    printer <<< """      case other : |LAYER_UC|_FieldIteratorAccess => other.dim == dim"""
    printer <<< """      case other : |LAYER_UC|_VariableAccess      => other.name == name"""
    printer <<< """      case _                              => super.equals(obj)"""
    printer <<< """    }"""
    printer <<< """  }"""
    printer <<< """}"""
    printer.toString
  }
}
