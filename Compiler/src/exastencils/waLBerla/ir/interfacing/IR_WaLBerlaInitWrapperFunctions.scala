package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaInitBlockForest
import exastencils.waLBerla.ir.communication.IR_WaLBerlaInitCommSchemes
import exastencils.waLBerla.ir.cuda.CUDA_WaLBerlaAddGPUFieldToStorage
import exastencils.waLBerla.ir.field._

object IR_WaLBerlaInitWrapperFunctions {
  var functions : ListBuffer[IR_FunctionLike] = ListBuffer()

  functions += IR_WaLBerlaInitBlockForest()
  for (field <- IR_WaLBerlaFieldCollection.objects.groupBy(_.name)) {
    val leveledFields = field._2.groupBy(_.level).map(_._2.head).to[ListBuffer]
    functions += IR_WaLBerlaAddFieldToStorage(leveledFields : _*)

    if (Knowledge.cuda_enabled)
      functions += CUDA_WaLBerlaAddGPUFieldToStorage(leveledFields : _*)

    if (Knowledge.waLBerla_generateCommSchemes) {
      functions += IR_WaLBerlaInitCommSchemes(onGPU = false, leveledFields : _*)

      if (Knowledge.cuda_enabled)
        functions += IR_WaLBerlaInitCommSchemes(onGPU = true, leveledFields : _*)
    }
  }
}
