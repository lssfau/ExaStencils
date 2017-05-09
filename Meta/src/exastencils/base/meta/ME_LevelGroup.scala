package exastencils.base.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_LevelGroup extends Generatable {
  override def validLayers() = ListBuffer(L2, L3, L4)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/base/|LAYER_LC|/|LAYER_UC|_LevelGroup.scala"

  override def generateForLayer(layer : Layer) = {
    val printer = new Printer
    printer <<< """package exastencils.base.|LAYER_LC|"""
    printer <<< """"""
    printer <<< """import scala.collection.mutable.HashSet"""
    printer <<< """"""
    if (L2 == layer || L3 == layer) {
      printer <<< """import exastencils.base.|NEXT_LC|._"""
    }
    printer <<< """import exastencils.logger.Logger"""
    printer <<< """import exastencils.prettyprinting.PpStream"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_LevelGroup"""
    printer <<< """"""
    printer <<< """trait |LAYER_UC|_LevelGroup extends |LAYER_UC|_DeclarationLevelSpecification {"""
    printer <<< """  override def resolveLevel : Int = Logger.error("Trying to resolve level for invalid type " + this.getClass.getName)"""
    printer <<< """}"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_AllLevels"""
    printer <<< """"""
    printer <<< """case object |LAYER_UC|_AllLevels extends |LAYER_UC|_LevelGroup {"""
    printer <<< """  exastencils.core.Duplicate.registerImmutable(this.getClass)"""
    printer <<< """  def prettyprint(out : PpStream) = out << "all""""
    if (L2 == layer || L3 == layer) {
      printer <<< """  override def progress = |NEXT_UC|_AllLevels"""
    }
    printer <<< """}"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_LevelRange"""
    printer <<< """"""
    printer <<< """case class |LAYER_UC|_LevelRange(var begin : |LAYER_UC|_LevelSpecification, var end : |LAYER_UC|_LevelSpecification) extends |LAYER_UC|_LevelGroup {"""
    printer <<< """  def prettyprint(out : PpStream) = out << '(' << begin << " to " << end << ')'"""
    if (L2 == layer || L3 == layer) {
      printer <<< """  override def progress = |NEXT_UC|_LevelRange(begin.progress, end.progress)"""
    }
    printer <<< """}"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_LevelList"""
    printer <<< """"""
    printer <<< """object |LAYER_UC|_LevelList {"""
    printer <<< """  def apply() = new |LAYER_UC|_LevelList(HashSet())"""
    printer <<< """  def apply(level : |LAYER_UC|_DeclarationLevelSpecification) = new |LAYER_UC|_LevelList(HashSet(level))"""
    printer <<< """  def apply(levels : List[|LAYER_UC|_DeclarationLevelSpecification]) = new |LAYER_UC|_LevelList(levels.to[HashSet])"""
    printer <<< """}"""
    printer <<< """"""
    printer <<< """case class |LAYER_UC|_LevelList(var levels : HashSet[|LAYER_UC|_DeclarationLevelSpecification]) extends |LAYER_UC|_LevelGroup {"""
    printer <<< """  override def prettyprint(out : PpStream) = out << '(' <<< (levels, ", ") << ')'"""
    if (L2 == layer || L3 == layer) {
      printer <<< """  override def progress = |NEXT_UC|_LevelList(levels.map(_.progress))"""
    }
    printer <<< """"""
    printer <<< """  def flatten() : Unit = {"""
    printer <<< """    levels.foreach {"""
    printer <<< """      case elem @ |LAYER_UC|_LevelList(x) =>"""
    printer <<< """        levels.++=(x)"""
    printer <<< """        levels.remove(elem)"""
    printer <<< """      case _                      =>"""
    printer <<< """    }"""
    printer <<< """  }"""
    printer <<< """"""
    printer <<< """  def contains(level : Int) = levels.exists({ case |LAYER_UC|_SingleLevel(`level`) => true; case _ => false })"""
    printer <<< """}"""
    printer <<< """"""
    printer <<< """/// |LAYER_UC|_NegatedLevelList"""
    printer <<< """"""
    printer <<< """object |LAYER_UC|_NegatedLevelList {"""
    printer <<< """  def apply() = new |LAYER_UC|_NegatedLevelList(|LAYER_UC|_LevelList())"""
    printer <<< """  def apply(level : |LAYER_UC|_DeclarationLevelSpecification) = new |LAYER_UC|_NegatedLevelList(|LAYER_UC|_LevelList(level))"""
    printer <<< """  def apply(levels : List[|LAYER_UC|_DeclarationLevelSpecification]) = new |LAYER_UC|_NegatedLevelList(|LAYER_UC|_LevelList(levels))"""
    printer <<< """}"""
    printer <<< """"""
    printer <<< """case class |LAYER_UC|_NegatedLevelList(var levels : |LAYER_UC|_LevelList) extends |LAYER_UC|_LevelGroup {"""
    printer <<< """  def prettyprint(out : PpStream) = out << "not" << levels"""
    if (L2 == layer || L3 == layer) {
      printer <<< """  override def progress = |NEXT_UC|_NegatedLevelList(levels.progress)"""
    }
    printer <<< """}"""
    printer.toString
  }
}
