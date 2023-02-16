package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_FunctionLike

// collection with setup functions for coupling
object IR_WaLBerlaInitCouplingWrapperFunctions {
  var functions : ListBuffer[IR_FunctionLike] = ListBuffer()

  functions += IR_WaLBerlaInitStaticRectDomain()
}