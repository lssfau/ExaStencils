package exastencils.parallelization.api.cuda

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.core.ObjectWithState
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.StatementList
import exastencils.util.ir.IR_RawPrint

/// CUDA_CheckError

object CUDA_CheckError extends ObjectWithState {
  var counter : Int = 0

  override def clear() = { counter = 0 }
}

case class CUDA_CheckError(var exp : IR_Expression) extends CUDA_HostStatement with IR_Expandable {

  import CUDA_CheckError._

  override def expand() : Output[StatementList] = {
    val statusName = s"cudaStatus_$counter"
    counter += 1
    def status = IR_VariableAccess(statusName, IR_SpecialDatatype("cudaError_t"))

    ListBuffer(
      IR_VariableDeclaration(status, exp),
      IR_IfCondition("cudaSuccess" Neq status,
        IR_RawPrint("\"CUDA error in file (\"", "__FILE__", "\"), line (\"", "__LINE__", "\"): \"", status,
          "\" -> \"", IR_FunctionCall("cudaGetErrorString", status), "std::endl")))
  }
}
