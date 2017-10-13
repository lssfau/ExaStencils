package exastencils.base

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Root
import exastencils.base.l1.L1_Root
import exastencils.base.l2.L2_Root
import exastencils.base.l3.L3_Root
import exastencils.base.l4.L4_Root
import exastencils.datastructures._

case object ExaRootNode extends RootNode {
  var l1_root : L1_Root = null
  var l2_root : L2_Root = null
  var l3_root : L3_Root = null
  var l4_root : L4_Root = null
  var ir_root : IR_Root = null

  // TODO: this should not be required // replace with sth like
  //    def nodes = l1_root.nodes ++ l2_root.nodes ++ etc 
  def nodes = myNodes
  private val myNodes = ListBuffer[Node]()

  def clear() = {
    l1_root = null
    l2_root = null
    l3_root = null
    l4_root = null
    ir_root = null
  }

  def mergeL1(newRoot : L1_Root) = {
    // merge nodes if previous root exists
    if (ExaRootNode.l1_root != null) newRoot.nodes ++= ExaRootNode.l1_root.nodes
    newRoot.flatten()

    ExaRootNode.l1_root = newRoot
  }

  def mergeL2(newRoot : L2_Root) = {
    // merge nodes if previous root exists
    if (ExaRootNode.l2_root != null) newRoot.nodes ++= ExaRootNode.l2_root.nodes
    newRoot.flatten()

    ExaRootNode.l2_root = newRoot
  }

  def mergeL3(newRoot : L3_Root) = {
    // merge nodes if previous root exists
    if (ExaRootNode.l3_root != null) newRoot.nodes ++= ExaRootNode.l3_root.nodes
    newRoot.flatten()

    ExaRootNode.l3_root = newRoot
  }

  def mergeL4(newRoot : L4_Root) = {
    // merge nodes if previous root exists
    if (ExaRootNode.l4_root != null) newRoot.nodes ++= ExaRootNode.l4_root.nodes
    newRoot.flatten()

    ExaRootNode.l4_root = newRoot
  }

  def mergeIR(newRoot : IR_Root) = {
    // merge nodes if previous root exists
    if (ExaRootNode.ir_root != null) newRoot.nodes ++= ExaRootNode.ir_root.nodes
    newRoot.flatten()

    ExaRootNode.ir_root = newRoot
  }

  def progressToL2() = {
    l2_root = l1_root.progress
    l1_root = null
  }

  def progressToL3() = {
    l3_root = l2_root.progress
    l2_root = null
  }

  def progressToL4() = {
    l4_root = l3_root.progress
    l3_root = null
  }

  def progressToIR() = {
    ir_root = l4_root.progress
    l4_root = null
  }
}
