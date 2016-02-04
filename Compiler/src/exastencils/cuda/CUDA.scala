package exastencils.cuda

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.prettyprinting._
import exastencils.util._

// TODO: introduce abstraction layer for general device interfaces

trait CUDA_Statement extends Statement

case class CUDA_Init() extends CUDA_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "cuInit(0);"
}

case class CUDA_Finalize() extends CUDA_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    // has to be done after all other de-initialization statements
    out << "cuCtxDestroy(cudaContext);"
  }
}

case class CUDA_CheckError(var exp : Expression) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CUDA_CheckError\n"

  override def expand : Output[Scope] = {
    // TODO: replace with define?
    Scope(ListBuffer[Statement](
      VariableDeclarationStatement(SpecialDatatype("cudaError_t"), "cudaStatus", Some(exp)),
      new ConditionStatement(NeqExpression("cudaStatus", "cudaSuccess"),
        PrintStatement(ListBuffer("\"CUDA error in file (\"", "__FILE__", "\"), line (\"", "__LINE__", "\"): \"", "cudaStatus",
          "\" -> \"", new FunctionCallExpression("cudaGetErrorString", "cudaStatus"), "std::endl")))))
  }
}

case class CUDA_AllocateStatement(var pointer : Expression, var numElements : Expression, var datatype : Datatype) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CUDA_AllocateStatement\n"

  override def expand : Output[Statement] = {
    CUDA_CheckError(
      FunctionCallExpression("cudaMalloc",
        ListBuffer[Expression](
          CastExpression(PointerDatatype(PointerDatatype(UnitDatatype)), AddressofExpression(pointer)),
          numElements * SizeOfExpression(datatype))))
  }
}

case class CUDA_FreeStatement(var pointer : Expression) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CUDA_FreeStatement\n"

  override def expand : Output[Statement] = {
    ExpressionStatement(new FunctionCallExpression("cudaFree", pointer))
  }
}

case class CUDA_UpdateHostData(var fieldAccess : FieldAccess) extends Statement with Expandable {
  // TODO: allow targeting of specific index ranges

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CUDA_UpdateHostData\n"

  override def expand : Output[ConditionStatement] = {
    val fieldSelection = fieldAccess.fieldSelection
    val field = fieldSelection.field
    // TODO: loop over frags
    new ConditionStatement(
      iv.DeviceDataUpdated(field, fieldSelection.slot),
      ListBuffer[Statement](
        CUDA_CheckError(
          FunctionCallExpression("cudaMemcpy", ListBuffer[Expression](
            iv.FieldData(field, fieldSelection.level, fieldSelection.slot),
            iv.FieldDeviceData(field, fieldSelection.level, fieldSelection.slot),
            (0 to Knowledge.dimensionality).map(dim => field.fieldLayout.idxById("TOT", dim)).reduceLeft(_ * _)
              * field.dataType.resolveFlattendSize * SizeOfExpression(field.dataType),
            "cudaMemcpyDeviceToHost"))),
        AssignmentStatement(iv.DeviceDataUpdated(field, fieldSelection.slot), BooleanConstant(false))))
  }
}

case class CUDA_UpdateDeviceData(var fieldAccess : FieldAccess) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CUDA_UpdateDeviceData\n"

  override def expand : Output[ConditionStatement] = {
    val fieldSelection = fieldAccess.fieldSelection
    val field = fieldSelection.field
    // TODO: loop over frags
    new ConditionStatement(
      iv.HostDataUpdated(field, fieldSelection.slot),
      ListBuffer[Statement](
        CUDA_CheckError(
          FunctionCallExpression("cudaMemcpy", ListBuffer[Expression](
            iv.FieldDeviceData(field, fieldSelection.level, fieldSelection.slot),
            iv.FieldData(field, fieldSelection.level, fieldSelection.slot),
            (0 to Knowledge.dimensionality).map(dim => field.fieldLayout.idxById("TOT", dim)).reduceLeft(_ * _)
              * field.dataType.resolveFlattendSize * SizeOfExpression(field.dataType),
            "cudaMemcpyHostToDevice"))),
        AssignmentStatement(iv.HostDataUpdated(field, fieldSelection.slot), BooleanConstant(false))))
  }
}

case class CUDA_FunctionCallExpression(var name : String, var arguments : ListBuffer[Expression]) extends Expression {
  def this(name : String, args : Expression*) = this(name, args.to[ListBuffer])

  // FIXME: allow setting thread and block parameters via class arguments
  override def prettyprint(out : PpStream) : Unit = out << name << "<<<" << "dim3(36, 36, 36)" << ", " << "dim3(8, 8, 8)" << ">>>" << '(' <<< (arguments, ", ") << ')'
}
