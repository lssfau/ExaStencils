package exastencils.waLBerla.ir

import exastencils.base.ir._
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.domain.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.util.ir.IR_StackCollector
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverBlocks

object IR_WaLBerlaReplaceFragmentIVs extends DefaultStrategy("Replace frag info accesses with accesses to waLBerla block info") {

  var collector = new IR_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  def block = IR_WaLBerlaLoopOverBlocks.defIt

  def inWaLBerlaBlockLoop(collector : IR_StackCollector) =
    collector.stack.exists {
      case _ : IR_WaLBerlaLoopOverBlocks              => true
      case _ : IR_WaLBerlaLoopOverPoints              => true
      case _ : IR_WaLBerlaLoopOverPointsInOneFragment => true
      case _ : IR_WaLBerlaLoopOverDimensions          => true
      case _                                          => false
    }

  def aabbDatatype = IR_SpecialDatatype("math::AABB")

  def getBlockAABB() = IR_MemberFunctionCallArrow(block, "getAABB", aabbDatatype)

  this += Transformation("Replace", {

    /* TODO
    case iv : IR_IV_FragmentId if inWaLBerlaScope(collector)       => ...
    case iv : IR_IV_FragmentIndex if inWaLBerlaScope(collector)    => ...
    case iv : IR_IV_IsValidForDomain if inWaLBerlaScope(collector) => ...
    */

    // TODO: association frags with blocks

    // TODO: fragment connection

    case iv @ IR_IV_FragmentPosition(dim, fragmentIdx) if inWaLBerlaBlockLoop(collector) =>
      IR_ArrayAccess(IR_MemberFunctionCall(getBlockAABB(), "center"), dim)

    case iv @ IR_IV_FragmentPositionBegin(dim, fragmentIdx) if inWaLBerlaBlockLoop(collector) =>
      IR_MemberFunctionCall(getBlockAABB(), "min", dim)

    case iv @ IR_IV_FragmentPositionEnd(dim, fragmentIdx) if inWaLBerlaBlockLoop(collector)  =>
      IR_MemberFunctionCall(getBlockAABB(), "max", dim)
  })
}
