package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_FunctionLike
import exastencils.config.Knowledge
import exastencils.waLBerla.ir.field.IR_WaLBerlaFieldCollection
import exastencils.waLBerla.ir.gpu.GPU_WaLBerlaSetGPUFlag

object IR_WaLBerlaSetterFunctionCollection {
  var functions : ListBuffer[IR_FunctionLike] = ListBuffer()

  if (Knowledge.cuda_enabled) {
    for (field <- IR_WaLBerlaFieldCollection.objects.groupBy(_.name).map(_._2.head)) {
      functions += GPU_WaLBerlaSetGPUFlag(field)
    }
  }
}
