package exastencils.base.meta

import scala.collection.mutable.ListBuffer

import meta._

object ME_Progressable extends Generatable {
  override def validLayers() = ListBuffer(L2, L3, L4)

  override def filenameForLayer(layer : Layer) = s"./Compiler/src/exastencils/base/|LAYER_LC|/|LAYER_UC|_Progressable.scala"

  override def generateForLayer(layer : Layer) = {
    """package exastencils.base.|LAYER_LC|

import exastencils.base.|NEXT_LC|.|NEXT_UC|_Node

/// |LAYER_UC|_Progressable

trait |LAYER_UC|_Progressable {
  def progress : |NEXT_UC|_Node
}

/// |LAYER_UC|_ProgressOption

object |LAYER_UC|_ProgressOption {
  def apply[|LAYER_UC|_Type <: |LAYER_UC|_Progressable, |NEXT_UC|_Type <: |NEXT_UC|_Node](toProgress : Option[|LAYER_UC|_Type])(progFct : (|LAYER_UC|_Type => |NEXT_UC|_Type)) : Option[|NEXT_UC|_Type] = {
    if (toProgress.isDefined)
      Some(progFct(toProgress.get))
    else
      None
  }
}
"""
  }
}
