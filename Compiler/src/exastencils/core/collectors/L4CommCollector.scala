package exastencils.core.collectors

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l4._
import scala.collection.mutable.Map

class L4CommCollector(var communicates : Map[FieldAccess, Int]) extends Collector {
  override def enter(node : Node) : Unit = {
    node match {
      case StencilConvolution(stencil, field) => communicates(field) = communicates.getOrElse(field, 0) max 1 // FIXME: get stencil radius
      case _                                  =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case _ =>
    }
  }

  override def reset() : Unit = {
    communicates.clear
  }
}
