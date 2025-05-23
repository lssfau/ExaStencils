package exastencils.waLBerla.l4.blockforest

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_Reduction
import exastencils.base.l4.L4_Statement
import exastencils.baseExt.l4.L4_LoopOverProcessLocalBlocks
import exastencils.parallelization.ir.IR_ParallelizationInfo
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverLocalBlocks

case class L4_WaLBerlaLoopOverBlocks(
    var body : ListBuffer[L4_Statement],
    var reduction : Option[L4_Reduction]) extends L4_LoopOverProcessLocalBlocks {

  override def prettyprint(out : PpStream) = {
    out << "loop over blocks "
    if (reduction.isDefined) out << "with " << reduction.get
    out << "{\n" <<< (body, "\n") << "\n}"
  }

  override def progress = ProgressLocation {
    // TODO: introduce L4_ParallelizationInfo
    val parallelization = IR_ParallelizationInfo()
    // assume parallelizabilty by default
    parallelization.potentiallyParallel = true
    parallelization.reduction = reduction.map(_.progress)

    IR_WaLBerlaLoopOverLocalBlocks(body.map(_.progress), parallelization)
  }
}
