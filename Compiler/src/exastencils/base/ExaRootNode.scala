package exastencils.base

import exastencils.datastructures.Node
import exastencils.datastructures.RootNode

import scala.collection.mutable.ListBuffer

case object ExaRootNode extends RootNode {
  var l1_root = None
  var l2_root : exastencils.base.l2.L2_Root = null
  var l3_root : exastencils.base.l3.L3_Root = null
  var l4_root : exastencils.base.l4.L4_Root = null
  var ir_root : exastencils.base.ir.IR_Root = null

  def nodes = mynodes
  private var mynodes = ListBuffer[Node]()

  def ProgressToL2() = {
    // ...
  }

  def ProgressToL3() = {
    // ...
    l2_root = null
  }

  def ProgressToL4() = {
    // ...
    l3_root = null
  }

  def ProgressToIR() = {
    ir_root = l4_root.progress
    l4_root = null
  }
}
