package exastencils.base.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_LevelAlias extends Generatable {
  override def validLayers() = ListBuffer(L2, L3, L4)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/base/|LAYER_LC|/|LAYER_UC|_LevelAlias.scala"

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
    printer <<< """/// |LAYER_UC|_LevelAlias"""
    printer <<< """"""
    printer <<< """trait |LAYER_UC|_LevelAlias extends |LAYER_UC|_DeclarationLevelSpecification with |LAYER_UC|_AccessLevelSpecification {"""
    printer <<< """  override def resolveLevel : Int = Logger.error("Trying to resolve level for invalid type " + this.getClass.getName)"""
    if (L2 == layer || L3 == layer) {
      printer <<< """  override def progress : |NEXT_UC|_DeclarationLevelSpecification with |NEXT_UC|_AccessLevelSpecification"""
    }
    printer <<< """}"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_CurrentLevel"""
    printer <<< """"""
    printer <<< """case object |LAYER_UC|_CurrentLevel extends |LAYER_UC|_LevelAlias {"""
    printer <<< """  exastencils.core.Duplicate.registerImmutable(this.getClass)"""
    printer <<< """  def prettyprint(out : PpStream) = out << "current""""
    if (L2 == layer || L3 == layer) {
      printer <<< """  override def progress = |NEXT_UC|_CurrentLevel"""
    }
    printer <<< """}"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_CoarserLevel"""
    printer <<< """"""
    printer <<< """case object |LAYER_UC|_CoarserLevel extends |LAYER_UC|_LevelAlias {"""
    printer <<< """  exastencils.core.Duplicate.registerImmutable(this.getClass)"""
    printer <<< """  def prettyprint(out : PpStream) = out << "coarser""""
    if (L2 == layer || L3 == layer) {
      printer <<< """  override def progress = |NEXT_UC|_CoarserLevel"""
    }
    printer <<< """}"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_FinerLevel"""
    printer <<< """"""
    printer <<< """case object |LAYER_UC|_FinerLevel extends |LAYER_UC|_LevelAlias {"""
    printer <<< """  exastencils.core.Duplicate.registerImmutable(this.getClass)"""
    printer <<< """  def prettyprint(out : PpStream) = out << "finer""""
    if (L2 == layer || L3 == layer) {
      printer <<< """  override def progress = |NEXT_UC|_FinerLevel"""
    }
    printer <<< """}"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_CoarsestLevel"""
    printer <<< """"""
    printer <<< """case object |LAYER_UC|_CoarsestLevel extends |LAYER_UC|_LevelAlias {"""
    printer <<< """  exastencils.core.Duplicate.registerImmutable(this.getClass)"""
    printer <<< """  def prettyprint(out : PpStream) = out << "coarsest""""
    if (L2 == layer || L3 == layer) {
      printer <<< """  override def progress = |NEXT_UC|_CoarsestLevel"""
    }
    printer <<< """}"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_FinestLevel"""
    printer <<< """"""
    printer <<< """case object |LAYER_UC|_FinestLevel extends |LAYER_UC|_LevelAlias {"""
    printer <<< """  exastencils.core.Duplicate.registerImmutable(this.getClass)"""
    printer <<< """  def prettyprint(out : PpStream) = out << "finest""""
    if (L2 == layer || L3 == layer) {
      printer <<< """  override def progress = |NEXT_UC|_FinestLevel"""
    }
    printer <<< """}"""
    printer.toString
  }
}
