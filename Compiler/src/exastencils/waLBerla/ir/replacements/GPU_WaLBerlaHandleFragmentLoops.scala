package exastencils.waLBerla.ir.replacements

import exastencils.base.ir.IR_Scope
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.parallelization.api.cuda._
import exastencils.waLBerla.ir.field.IR_WaLBerlaField
import exastencils.waLBerla.ir.gpu._

/// GPU_WaLBerlaHandleFragmentLoops

object GPU_WaLBerlaHandleFragmentLoops extends IR_WaLBerlaReplacementStrategy("Perform handling of fragment loops and replace GPU reduction IVs") {

  object ReplaceReductionTmps extends QuietDefaultStrategy("Replace GPU reduction IVs to wb counterparts") {
    this += Transformation("..", {
      // reduction tmps
      case _ @ CUDA_MatrixDeviceCopy(name, baseDt, size, fragmentIdx)              =>
        GPU_WaLBerlaMatrixDeviceCopy("wb_" + name, baseDt, size, fragmentIdx)
      case _ @ CUDA_ManagedReductionResultPointer(name, baseDt, size, fragmentIdx) =>
        GPU_WaLBerlaManagedReductionResultPointer("wb_" + name, baseDt, size, fragmentIdx)
    })
  }

  this += Transformation("..", {
    case handle : CUDA_HandleFragmentLoops if handle.fieldAccesses.exists(_._2.field.isInstanceOf[IR_WaLBerlaField]) =>
      val stmts = handle.expandSpecial()
      ReplaceReductionTmps.applyStandalone(IR_Scope(stmts))

      stmts
  })
}
