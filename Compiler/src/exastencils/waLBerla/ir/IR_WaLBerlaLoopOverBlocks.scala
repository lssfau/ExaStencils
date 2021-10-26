package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.domain.ir._
import exastencils.parallelization.ir._
import exastencils.util.ir.IR_StackCollector
import exastencils.waLBerla.ir.IR_WaLBerlaUtil.getBlocks

/// IR_WaLBerlaLoopOverBlocks

object IR_WaLBerlaLoopOverBlocks {
  def defIt = IR_VariableAccess("block", IR_SpecialDatatype("auto"))
}

// iterates through process-local blocks
case class IR_WaLBerlaLoopOverBlocks(
    var body : ListBuffer[IR_Statement],
    var parallelization : IR_ParallelizationInfo = IR_ParallelizationInfo()) extends IR_ScopedStatement with IR_SpecialExpandable with IR_HasParallelizationInfo {

  def expandSpecial() : Output[IR_ForLoop] = {
    // TODO: separate omp and potentiallyParallel
    parallelization.potentiallyParallel = Knowledge.omp_enabled && Knowledge.omp_parallelizeLoopOverFragments && parallelization.potentiallyParallel

    // collect fields accessed in loop
    var fieldsAccessed = ListBuffer[IR_WaLBerlaField]()
    IR_CollectAccessedWaLBerlaFields.applyStandalone(body)
    fieldsAccessed ++= Duplicate(IR_CollectAccessedWaLBerlaFields.wbFieldAccesses).groupBy(_.name).map(_._2.head)

    import IR_WaLBerlaLoopOverBlocks._

    // TODO for multiple waLBerla blocks and exa fragments: association between them

    new IR_ForLoop(
      IR_VariableDeclaration(defIt, IR_MemberFunctionCallArrow(getBlocks, "begin", defIt.datatype)),
      IR_Neq(defIt, IR_MemberFunctionCallArrow(getBlocks, "end", defIt.datatype)),
      IR_ExpressionStatement(IR_PreIncrement(defIt)),
      IR_WaLBerlaUtil.getFields(fieldsAccessed : _*) ++ body,
      parallelization)
  }
}

object IR_WaLBerlaResolveLoopOverBlocks extends DefaultStrategy("Resolve waLBerla LoopOverBlocks") {

  // replace frag info ivs first
  var collector = new IR_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  def inWaLBerlaBlockLoop(collector : IR_StackCollector) =
    collector.stack.exists(n => n.isInstanceOf[IR_WaLBerlaLoopOverBlocks]) &&
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

  // resolve
  this += Transformation("Resolve", {
    case loop : IR_WaLBerlaLoopOverBlocks => loop.expandSpecial()
  })
}
