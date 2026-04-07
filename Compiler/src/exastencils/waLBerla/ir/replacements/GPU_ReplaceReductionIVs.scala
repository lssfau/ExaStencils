package exastencils.waLBerla.ir.replacements

import exastencils.base.ir.IR_Scope
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.parallelization.api.cuda._
import exastencils.waLBerla.ir.field.IR_WaLBerlaField
import exastencils.waLBerla.ir.gpu._

/// GPU_ReplaceReductionIVs

object GPU_ReplaceReductionIVs extends IR_WaLBerlaReplacementStrategy("Replace GPU reduction IVs with waLBerla counterparts") {

  object ReplaceReductionTmps extends QuietDefaultStrategy("Replace reduction tmps for GPU") {
    this += Transformation("..", {
      // reduction tmps
      case _ @ CUDA_MatrixDeviceCopy(name, baseDt, size, fragmentIdx)      =>
        GPU_WaLBerlaMatrixDeviceCopy("wb_" + name, baseDt, size, fragmentIdx)
      case _ @ CUDA_ReductionResultBuffer(name, baseDt, size, fragmentIdx) =>
        GPU_WaLBerlaReductionResultBuffer("wb_" + name, baseDt, size, fragmentIdx)
      case _ @ CUDA_ReductionFragmentCopy(name, baseDt, fragmentIdx)       =>
        GPU_WaLBerlaReductionFragmentCopy("wb_" + name, baseDt, fragmentIdx)
    })
  }

  this += Transformation("..", {
    case handle : CUDA_HandleFragmentLoops if handle.fieldAccesses.exists(_._2.field.isInstanceOf[IR_WaLBerlaField]) =>
      val stmts = handle.expandSpecial()
      ReplaceReductionTmps.applyStandalone(IR_Scope(stmts))

      stmts
  })
}
