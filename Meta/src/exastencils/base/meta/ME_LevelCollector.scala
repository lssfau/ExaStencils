package exastencils.base.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_LevelCollector extends Generatable {
  override def validLayers() = ListBuffer(L2, L3, L4)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/base/|LAYER_LC|/|LAYER_UC|_LevelCollector.scala"

  override def generateForLayer(layer : Layer) = {
    val printer = new Printer
    printer <<< """package exastencils.base.|LAYER_LC|"""
    printer <<< """"""
    printer <<< """import scala.collection.mutable.Stack"""
    printer <<< """"""
    printer <<< """import exastencils.core.collectors.Collector"""
    printer <<< """import exastencils.datastructures.Node"""
    if (L2 == layer) {
      printer <<< """import exastencils.knowledge.|LAYER_LC|._"""
    }
    if (L3 == layer) {
      printer <<< """import exastencils.knowledge.|LAYER_LC|.|LAYER_UC|_LeveledKnowledgeDecl"""
    }
    printer <<< """import exastencils.logger.Logger"""
    printer <<< """"""
    if (L2 == layer) {
      printer <<< """/// |LAYER_UC|_LevelCollector"""
      printer <<< """"""
    }
    if (L3 == layer) {
      printer <<< """/// |LAYER_UC|_LevelCollector"""
      printer <<< """"""
    }
    printer <<< """class |LAYER_UC|_LevelCollector extends Collector {"""
    printer <<< """  private val levelStack = new Stack[Int]"""
    printer <<< """"""
    if (L2 == layer) {
      printer <<< """  def enterLevel(level : Option[|LAYER_UC|_LevelSpecification]) = {"""
      printer <<< """    level match {"""
      printer <<< """      case Some(|LAYER_UC|_SingleLevel(lvl)) => levelStack.push(lvl)"""
      printer <<< """      case _                         =>"""
      printer <<< """    }"""
      printer <<< """  }"""
      printer <<< """  def leaveLevel(level : Option[|LAYER_UC|_LevelSpecification]) = {"""
      printer <<< """    level match {"""
      printer <<< """      case Some(|LAYER_UC|_SingleLevel(_)) => levelStack.pop()"""
      printer <<< """      case _                       =>"""
      printer <<< """    }"""
      printer <<< """  }"""
      printer <<< """"""
    }
    if (L3 == layer) {
      printer <<< """  def enterLevel(level : Option[|LAYER_UC|_LevelSpecification]) = {"""
      printer <<< """    level match {"""
      printer <<< """      case Some(|LAYER_UC|_SingleLevel(lvl)) => levelStack.push(lvl)"""
      printer <<< """      case _                         =>"""
      printer <<< """    }"""
      printer <<< """  }"""
      printer <<< """  def leaveLevel(level : Option[|LAYER_UC|_LevelSpecification]) = {"""
      printer <<< """    level match {"""
      printer <<< """      case Some(|LAYER_UC|_SingleLevel(_)) => levelStack.pop()"""
      printer <<< """      case _                       =>"""
      printer <<< """    }"""
      printer <<< """  }"""
      printer <<< """"""
    }
    printer <<< """  override def enter(node : Node) : Unit = {"""
    printer <<< """    node match {"""
    if (L4 == layer) {
      printer <<< """      case |LAYER_UC|_Function(|LAYER_UC|_LeveledIdentifier(_, level), _, _, _, _) => levelStack.push(level.resolveLevel)"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """      case decl : |LAYER_UC|_LeveledKnowledgeDecl => enterLevel(decl.levels)"""
    }
    if (L3 == layer) {
      printer <<< """      case fct : |LAYER_UC|_Function              => enterLevel(fct.levels)"""
    }
    if (L4 == layer) {
      printer <<< """      case _                                                       =>"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """      case _                              =>"""
    }
    printer <<< """    }"""
    printer <<< """  }"""
    printer <<< """"""
    printer <<< """  override def leave(node : Node) : Unit = {"""
    printer <<< """    node match {"""
    if (L2 == layer || L3 == layer) {
      printer <<< """      case decl : |LAYER_UC|_LeveledKnowledgeDecl => leaveLevel(decl.levels)"""
    }
    if (L4 == layer) {
      printer <<< """      case |LAYER_UC|_Function(|LAYER_UC|_LeveledIdentifier(_, level), _, _, _, _) => levelStack.pop"""
    }
    if (L3 == layer) {
      printer <<< """      case fct : |LAYER_UC|_Function              => leaveLevel(fct.levels)"""
    }
    if (L4 == layer) {
      printer <<< """      case _                                                       =>"""
    }
    if (L2 == layer || L3 == layer) {
      printer <<< """      case _                              =>"""
    }
    printer <<< """    }"""
    printer <<< """  }"""
    printer <<< """"""
    printer <<< """  override def reset() : Unit = {"""
    printer <<< """    levelStack.clear"""
    printer <<< """  }"""
    printer <<< """"""
    printer <<< """  def inLevelScope : Boolean = levelStack.nonEmpty"""
    printer <<< """"""
    printer <<< """  def getCurrentLevel : Int = {"""
    printer <<< """    if (levelStack.isEmpty) {"""
    printer <<< """      Logger.dbg("Trying to access level outside of a valid scope")"""
    printer <<< """      -1"""
    printer <<< """    } else {"""
    printer <<< """      levelStack.head"""
    printer <<< """    }"""
    printer <<< """  }"""
    printer <<< """}"""
    printer.toString
  }
}
