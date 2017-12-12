package exastencils.parallelization.api.cuda

import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.core.collectors.Collector
import exastencils.datastructures._
import exastencils.field.ir._

class CUDA_GatherFieldAccess extends Collector {

  /** constants for read/write annotations */
  private final object Access extends Enumeration {
    type Access = Value
    val ANNOT : String = "CUDAAcc"
    val READ, WRITE, UPDATE = Value

    exastencils.core.Duplicate.registerConstant(this)
  }

  val fieldAccesses = HashMap[String, IR_MultiDimFieldAccess]()
  private var isRead : Boolean = true
  private var isWrite : Boolean = false

  override def reset() : Unit = {
    fieldAccesses.clear()
    isRead = true
    isWrite = false
  }

  override def enter(node : Node) : Unit = {

    node.getAnnotation(Access.ANNOT) match {
      case Some(Access.READ)   =>
        isRead = true
        isWrite = false
      case Some(Access.WRITE)  =>
        isRead = false
        isWrite = true
      case Some(Access.UPDATE) =>
        isRead = true
        isWrite = true
      case None                =>
    }

    node match {
      case assign : IR_Assignment =>
        assign.op match {
          case "=" => assign.dest.annotate(Access.ANNOT, Access.WRITE)
          case _   => assign.dest.annotate(Access.ANNOT, Access.UPDATE)
        }
        assign.src.annotate(Access.ANNOT, Access.READ)

      case access : IR_MultiDimFieldAccess =>
        val field = access.fieldSelection.field
        var identifier = field.codeName

        // TODO: array fields
        if (field.numSlots > 1) {
          access.fieldSelection.slot match {
            case IR_SlotAccess(_, offset) => identifier += s"_o$offset"
            case IR_IntegerConstant(slot) => identifier += s"_s$slot"
            case other                    => identifier += s"_s${ other.prettyprint }"
          }
        }

        if (isRead)
          fieldAccesses.put("read_" + identifier, access)
        if (isWrite)
          fieldAccesses.put("write_" + identifier, access)

      case _ =>
    }
  }

  override def leave(node : Node) : Unit = {
    if (node.removeAnnotation(Access.ANNOT).isDefined) {
      isRead = true
      isWrite = false
    }
  }
}
