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
    printer <<< """import exastencils.logger.Logger"""
    printer <<< """"""
    printer <<< """class |LAYER_UC|_LevelCollector extends Collector {"""
    printer <<< """  private val levelStack = new Stack[Int]"""
    printer <<< """"""
    printer <<< """  override def enter(node : Node) : Unit = {"""
    printer <<< """    node match {"""
    if (L2 == layer) {
      printer <<< """      // TODO"""
    }
    if (L4 == layer) {
      printer <<< """      case fct @ |LAYER_UC|_Function(|LAYER_UC|_LeveledIdentifier(_, level), _, _, _, _) => levelStack.push(level.resolveLevel)"""
    }
    if (L3 == layer) {
      printer <<< """      case fct @ |LAYER_UC|_Function(_, Some(level), _, _, _) => levelStack.push(level.resolveLevel)"""
    }
    if (L4 == layer) {
      printer <<< """      case _                                                             =>"""
    }
    if (L3 == layer) {
      printer <<< """      case _                                          =>"""
    }
    if (L2 == layer) {
      printer <<< """      case _ =>"""
    }
    printer <<< """    }"""
    printer <<< """  }"""
    printer <<< """"""
    printer <<< """  override def leave(node : Node) : Unit = {"""
    printer <<< """    node match {"""
    if (L4 == layer) {
      printer <<< """      case fct @ |LAYER_UC|_Function(|LAYER_UC|_LeveledIdentifier(_, level), _, _, _, _) => levelStack.pop"""
    }
    if (L2 == layer) {
      printer <<< """      // TODO"""
    }
    if (L3 == layer) {
      printer <<< """      case fct @ |LAYER_UC|_Function(_, Some(level), _, _, _) => levelStack.pop"""
    }
    if (L4 == layer) {
      printer <<< """      case _                                                             =>"""
    }
    if (L3 == layer) {
      printer <<< """      case _                                          =>"""
    }
    if (L2 == layer) {
      printer <<< """      case _ =>"""
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
