package exastencils.parallelization.api.cuda

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.datastructures.Transformation.Output
import exastencils.util.ir.IR_RawPrint

/// CUDA_CheckError

case class CUDA_CheckError(var exp : IR_Expression) extends CUDA_HostStatement with IR_Expandable {
  override def expand() : Output[IR_Scope] = {
    // TODO: replace with define?
    IR_Scope(
      IR_VariableDeclaration(IR_SpecialDatatype("cudaError_t"), "cudaStatus", Some(exp)),
      IR_IfCondition(IR_Neq("cudaStatus", "cudaSuccess"),
        IR_RawPrint("\"CUDA error in file (\"", "__FILE__", "\"), line (\"", "__LINE__", "\"): \"", "cudaStatus",
          "\" -> \"", IR_FunctionCall("cudaGetErrorString", "cudaStatus"), "std::endl")))
  }
}
