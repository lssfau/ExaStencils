package exastencils.grid

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._

/// GridGeometry_uniform_staggered_AA

object GridGeometry_uniform_staggered_AA extends GridGeometry_uniform with GridGeometry_staggered {
  // direct accesses
  override def stagCVWidth(level : Int, index : IR_ExpressionIndex, arrayIndex : Option[Int], dim : Int) = {
    // TODO: this introduces a slight extension at the physical boundary in the stagger dimension -> how to handle this? relevant or neglectable?
    0.5 * (cellWidth(level, GridUtil.offsetIndex(index, -1, dim), arrayIndex, dim) + cellWidth(level, index, arrayIndex, dim))
  }

  // nothing else to do here since everything can be pre-computed/ inlined
  override def initL4() = {}
  override def generateInitCode() = ListBuffer()
}

