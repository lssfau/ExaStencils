package exastencils.waLBerla.ir

import exastencils.base.ir.IR_Function
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.util.ir.IR_StackCollector

object IR_WaLBerlaHandleInlining extends DefaultStrategy("Handle inlining for waLBerla functors within functions") {
  val collector = new IR_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Handle", {
    case functor : IR_WaLBerlaFunctor =>
      val f : Option[IR_Function] = collector.stack.collectFirst{ case e : IR_Function => e }
      if (f.isDefined)
        f.get.allowInlining = false

      functor
  })
}
