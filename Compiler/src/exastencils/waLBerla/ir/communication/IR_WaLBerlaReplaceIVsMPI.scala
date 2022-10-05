package exastencils.waLBerla.ir.communication

import exastencils.base.ir._
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.parallelization.api.mpi._
import exastencils.util.ir.IR_StackCollector
import exastencils.waLBerla.ir.interfacing._

object IR_WaLBerlaReplaceIVsMPI extends DefaultStrategy("Replace exa's mpi data structures with the ones from waLBerla") {

  var collector = new IR_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  def inWaLBerlaScope(collector : IR_StackCollector) =
    collector.stack.exists(n => n.isInstanceOf[IR_WaLBerlaFunction] || n.isInstanceOf[IR_WaLBerlaFutureFunction])

  this += Transformation("Utilize waLBerla's MPI comm datastructures", {
    case iv : MPI_IV if iv == MPI_IV_MpiRank && inWaLBerlaScope(collector) =>
      IR_MemberFunctionCallArrowWithDt(IR_VariableAccess("MPIManager::instance()", IR_UnknownDatatype), "rank", iv.datatype)
    case iv : MPI_IV if iv == MPI_IV_MpiSize && inWaLBerlaScope(collector) =>
      IR_MemberFunctionCallArrowWithDt(IR_VariableAccess("MPIManager::instance()", IR_UnknownDatatype), "numProcesses", iv.datatype)
  })
}
