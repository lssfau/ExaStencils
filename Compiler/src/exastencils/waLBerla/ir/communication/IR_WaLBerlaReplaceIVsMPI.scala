package exastencils.waLBerla.ir.communication

import exastencils.base.ir._
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.parallelization.api.mpi._
import exastencils.util.ir.IR_StackCollector
import exastencils.waLBerla.ir.IR_WaLBerlaFunction
import exastencils.waLBerla.ir.IR_WaLBerlaFutureFunction

object IR_WaLBerlaReplaceIVsMPI extends DefaultStrategy("Replace exa's mpi data structures with the ones from waLBerla") {

  var collector = new IR_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  def inWaLBerlaScope(collector : IR_StackCollector) =
    collector.stack.exists(n => n.isInstanceOf[IR_WaLBerlaFunction] || n.isInstanceOf[IR_WaLBerlaFutureFunction])

  this += Transformation("Utilize waLBerla's MPI comm datastructures", {
    case iv : MPI_IV if inWaLBerlaScope(collector) =>
      val funcName = iv match {
        case MPI_IV_MpiRank => "rank"
        case MPI_IV_MpiSize => "numProcesses"
        case MPI_IV_MpiComm => "comm"
      }

      IR_MemberFunctionCallArrow(IR_VariableAccess("MPIManager::instance()", IR_UnknownDatatype), funcName, iv.datatype)
  })
}
