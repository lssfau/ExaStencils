package exastencils.base.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_Node extends Generatable {
  override def validLayers() = ListBuffer(L2, L3, L4, IR)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/base/|LAYER_LC|/|LAYER_UC|_Node.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.base.|LAYER_LC|

import exastencils.datastructures.Node

trait |LAYER_UC|_Node extends Node
"""
  }
}
