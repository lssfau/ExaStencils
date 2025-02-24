package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaInitUniformBlockForest
import exastencils.waLBerla.ir.communication.IR_WaLBerlaInitCommSchemes
import exastencils.waLBerla.ir.gpu.GPU_WaLBerlaAddGPUFieldToStorage
import exastencils.waLBerla.ir.field._

object IR_WaLBerlaInitWrapperFunctions {
  var functions : ListBuffer[IR_FunctionLike] = ListBuffer()

  functions += IR_WaLBerlaInitUniformBlockForest()
  for (field <- IR_WaLBerlaFieldCollection.objects.groupBy(_.name)) {
    val leveledFields = field._2.groupBy(_.level).map(_._2.head).to[ListBuffer]

    if (Knowledge.waLBerla_cacheFieldPointers) {
      // init field instance pointers
      functions += IR_WaLBerlaInitFieldInstances(onGPU = false, leveledFields : _*)
      if (Knowledge.cuda_enabled)
        functions += IR_WaLBerlaInitFieldInstances(onGPU = true, leveledFields : _*)

      // store and init pointers to internal waLBerla field data
      if (Knowledge.waLBerla_useInternalMemoryPointers) {
        functions += IR_WaLBerlaInitFieldDataPtrs(onGPU = false, leveledFields : _*)
        if (Knowledge.cuda_enabled)
          functions += IR_WaLBerlaInitFieldDataPtrs(onGPU = true, leveledFields : _*)
      }
    }

    // init comm scheme objects
    if (Knowledge.waLBerla_generateCommSchemes) {
      functions += IR_WaLBerlaInitCommSchemes(onGPU = false, leveledFields : _*)
      if (Knowledge.cuda_enabled)
        functions += IR_WaLBerlaInitCommSchemes(onGPU = true, leveledFields : _*)
    }
  }
}
