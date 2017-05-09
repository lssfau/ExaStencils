package exastencils.base.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_LevelSingle extends Generatable {
  override def validLayers() = ListBuffer(L2, L3, L4)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/base/|LAYER_LC|/|LAYER_UC|_LevelSingle.scala"

  override def generateForLayer(layer : Layer) = {
    val printer = new Printer
    printer <<< """package exastencils.base.|LAYER_LC|"""
    printer <<< """"""
    if (L2 == layer || L3 == layer) {
      printer <<< """import exastencils.base.|NEXT_LC|._"""
    }
    printer <<< """import exastencils.logger.Logger"""
    printer <<< """import exastencils.prettyprinting.PpStream"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_SingleLevel"""
    printer <<< """"""
    printer <<< """case class |LAYER_UC|_SingleLevel(var level : Int) extends |LAYER_UC|_DeclarationLevelSpecification with |LAYER_UC|_AccessLevelSpecification {"""
    printer <<< """  def prettyprint(out : PpStream) = out << level"""
    printer <<< """  override def toString = level.toString"""
    printer <<< """  override def resolveLevel : Int = level"""
    if (L2 == layer || L3 == layer) {
      printer <<< """  override def progress = |NEXT_UC|_SingleLevel(level)"""
    }
    printer <<< """}"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_RelativeLevel"""
    printer <<< """"""
    printer <<< """// FIXME: op -> BinOp"""
    printer <<< """case class |LAYER_UC|_RelativeLevel(var base : |LAYER_UC|_LevelSpecification, var op : String, var offset : Int) extends |LAYER_UC|_DeclarationLevelSpecification with |LAYER_UC|_AccessLevelSpecification {"""
    printer <<< """  def prettyprint(out : PpStream) = out << '(' << base << ' ' << op << ' ' << offset << ')'"""
    printer <<< """  override def resolveLevel : Int = Logger.error("Trying to resolve level for invalid type " + this.getClass.getName)"""
    if (L2 == layer || L3 == layer) {
      printer <<< """  override def progress = |NEXT_UC|_RelativeLevel(base.progress, op, offset)"""
    }
    printer <<< """}"""
    printer.toString
  }
}
