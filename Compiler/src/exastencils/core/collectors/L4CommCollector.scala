package exastencils.core.collectors

import scala.collection.mutable.Map

import exastencils.datastructures._
import exastencils.field.l4.L4_FieldAccess
import exastencils.operator.l4.L4_OperatorTimesField

class L4CommCollector(var communicates : Map[L4_FieldAccess, Int]) extends Collector {
  override def enter(node : Node) : Unit = {
    node match {
      case L4_OperatorTimesField(op, field) => communicates(field) = communicates.getOrElse(field, 0) max 1 // FIXME: get stencil radius
      case _                                =>
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
