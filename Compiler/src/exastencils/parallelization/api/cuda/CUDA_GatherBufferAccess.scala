package exastencils.parallelization.api.cuda

import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.communication.ir._
import exastencils.core.collectors.Collector
import exastencils.datastructures._

class CUDA_GatherBufferAccess extends Collector {

  /** constants for read/write annotations */
  private final object Access extends Enumeration {
    type Access = Value
    val ANNOT : String = "CUDAAcc"
    val READ, WRITE, UPDATE = Value

    exastencils.core.Duplicate.registerConstant(this)
  }

  val bufferAccesses = HashMap[String, IR_IV_CommBuffer]()
  private var isRead : Boolean = true
  private var isWrite : Boolean = false

  override def reset() : Unit = {
    bufferAccesses.clear()
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

      case buffer : IR_IV_CommBuffer =>
        val identifier = buffer.resolveName()

        if (isRead)
          bufferAccesses.put("read_" + identifier, buffer)
        if (isWrite)
          bufferAccesses.put("write_" + identifier, buffer)

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
