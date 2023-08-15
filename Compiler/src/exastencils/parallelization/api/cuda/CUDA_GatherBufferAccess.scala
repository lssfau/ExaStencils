//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.parallelization.api.cuda

import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.communication.ir._
import exastencils.core.collectors.Collector
import exastencils.datastructures._
import exastencils.logger.Logger

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
      case _                   => Logger.error("Invalid annotation")
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
        val neighIdx = buffer.neighIdx

        if (isRead)
          bufferAccesses.put(s"read_${identifier}_{$neighIdx}", buffer)
        if (isWrite)
          bufferAccesses.put(s"write_${identifier}_{$neighIdx}", buffer)

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
