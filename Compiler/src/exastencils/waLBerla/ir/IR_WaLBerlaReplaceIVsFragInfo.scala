package exastencils.waLBerla.ir

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.domain.ir._
import exastencils.util.ir.IR_StackCollector

object IR_WaLBerlaReplaceIVsFragInfo extends DefaultStrategy("Replace accesses to fragment info IVs with waLBerla's structures") {

  var collector = new IR_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  def inWaLBerlaBlockLoop(collector : IR_StackCollector) =
    collector.stack.exists(n => n.isInstanceOf[IR_WaLBerlaFunction] || n.isInstanceOf[IR_WaLBerlaFutureFunction])


  def getAABB() = IR_MemberFunctionCallArrow(IR_WaLBerlaLoopOverBlocks.defIt, "getAABB", IR_SpecialDatatype("math::AABB"))

  this += Transformation("Replace frag info accesses with accesses to waLBerla block info", {

    /* TODO
    case iv : IR_IV_FragmentId if inWaLBerlaScope(collector)       => ...
    case iv : IR_IV_FragmentIndex if inWaLBerlaScope(collector)    => ...
    case iv : IR_IV_IsValidForDomain if inWaLBerlaScope(collector) => ...
    */

    // TODO: association frags with blocks

    // TODO: fragment connection

    case _ @ IR_IV_FragmentPosition(dim, fragmentIdx) if inWaLBerlaBlockLoop(collector) =>
      val getCenter = IR_MemberFunctionCall(getAABB(), "center")

      IR_ArrayAccess(getCenter, dim)

    case _ @ IR_IV_FragmentPositionBegin(dim, fragmentIdx) if inWaLBerlaBlockLoop(collector) =>
      IR_MemberFunctionCall(getAABB(), "min", dim)

    case _ @ IR_IV_FragmentPositionEnd(dim, fragmentIdx) if inWaLBerlaBlockLoop(collector) =>
      IR_MemberFunctionCall(getAABB(), "max", dim)
  })
}
