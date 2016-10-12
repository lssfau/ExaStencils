package exastencils.cuda

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.datastructures.Transformation.Output
import exastencils.prettyprinting.PpStream

/// CUDA_Allocate

case class CUDA_Allocate(var pointer : IR_Expression, var numElements : IR_Expression, var datatype : IR_Datatype) extends CUDA_HostStatement with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() : Output[IR_Statement] = {
    CUDA_CheckError(
      IR_FunctionCall("cudaMalloc",
        IR_Cast(IR_PointerDatatype(IR_PointerDatatype(IR_UnitDatatype)), IR_AddressofExpression(pointer)),
        numElements * IR_SizeOf(datatype)))
  }
}

/// CUDA_Free

case class CUDA_Free(var pointer : IR_Expression) extends CUDA_HostStatement with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() : Output[IR_Statement] = {
    IR_ExpressionStatement(IR_FunctionCall("cudaFree", pointer))
  }
}

/// CUDA_Memcpy

case class CUDA_Memcpy(var dest : IR_Expression, var src : IR_Expression, var sizeInBytes : IR_Expression, var direction : String) extends CUDA_HostStatement with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() : Output[IR_Statement] = {
    CUDA_CheckError(IR_FunctionCall("cudaMemcpy", dest, src, sizeInBytes, direction))
  }
}

/// CUDA_Memset

case class CUDA_Memset(var data : IR_Expression, var value : IR_Expression, var numElements : IR_Expression, var datatype : IR_Datatype) extends CUDA_HostStatement with IR_Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  override def expand() : Output[IR_Statement] = {
    CUDA_CheckError(IR_FunctionCall("cudaMemset", data, value, numElements * IR_SizeOf(datatype)))
  }
}
