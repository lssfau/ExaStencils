package exastencils.cuda

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.logger._
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

case class CUDA_UpdateHostData(var fieldAccess : FieldAccessLike) extends Statement with Expandable {
  // TODO: allow targeting of specific index ranges

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CUDA_UpdateHostData\n"

  override def expand : Output[ConditionStatement] = {
    val fieldSelection = fieldAccess.fieldSelection
    val field = fieldSelection.field
    new ConditionStatement(
      iv.DeviceDataUpdated(field, fieldSelection.slot),
      ListBuffer[Statement](
        CUDA_Memcpy(
          iv.FieldData(field, fieldSelection.level, fieldSelection.slot),
          iv.FieldDeviceData(field, fieldSelection.level, fieldSelection.slot),
          (0 until field.fieldLayout.numDimsData).map(dim => field.fieldLayout.idxById("TOT", dim)).reduceLeft(_ * _)
            * SizeOfExpression(field.resolveBaseDatatype),
          "cudaMemcpyDeviceToHost"),
        AssignmentStatement(iv.DeviceDataUpdated(field, fieldSelection.slot), BooleanConstant(false))))
  }
}

case class CUDA_UpdateDeviceData(var fieldAccess : FieldAccessLike) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CUDA_UpdateDeviceData\n"

  override def expand : Output[ConditionStatement] = {
    val fieldSelection = fieldAccess.fieldSelection
    val field = fieldSelection.field
    new ConditionStatement(
      iv.HostDataUpdated(field, fieldSelection.slot),
      ListBuffer[Statement](
        CUDA_Memcpy(
          iv.FieldDeviceData(field, fieldSelection.level, fieldSelection.slot),
          iv.FieldData(field, fieldSelection.level, fieldSelection.slot),
          (0 until field.fieldLayout.numDimsData).map(dim => field.fieldLayout.idxById("TOT", dim)).reduceLeft(_ * _)
            * SizeOfExpression(field.resolveBaseDatatype),
          "cudaMemcpyHostToDevice"),
        AssignmentStatement(iv.HostDataUpdated(field, fieldSelection.slot), BooleanConstant(false))))
  }
}

case class CUDA_FunctionCallExpression(
    var name : String,
    var arguments : ListBuffer[Expression],
    var numThreadsPerDim : Array[Expression],
    var numBlocksPerDim : Array[Expression] = Knowledge.experimental_cuda_blockSizeAsVec.map(n => n : Expression)) extends Expression {

  def this(name : String, arguments : ListBuffer[Expression], numThreadsPerDim : Array[Long]) = this(name, arguments, numThreadsPerDim.map(n => n : Expression))
  def this(name : String, arguments : ListBuffer[Expression], numThreadsPerDim : Array[Long], numBlocksPerDim : Array[Long]) = this(name, arguments, numThreadsPerDim.map(n => n : Expression), numBlocksPerDim.map(n => n : Expression))

  override def prettyprint(out : PpStream) : Unit = {
    val numDims = numThreadsPerDim.size
    if (numDims > 3) Logger.warn(s"${numDims}D kernel found; this is currently unsupported by CUDA") // TODO: check relation to compute capability

    val numBlocks = (0 until numDims).map(dim => {
      (numThreadsPerDim(dim) + numBlocksPerDim(dim) - 1) / numBlocksPerDim(dim)
    }).toArray

    // TODO: simplify? check if integer ops are handled correctly

    out << name << "<<<"
    if (1 == numDims)
      out << numBlocks(0) << ", " << numBlocksPerDim(0) // only one dimensions -> wrapping not necessary
    else
      out << s"dim$numDims(" <<< (numBlocks, ", ") << "), " << s"dim$numDims(" <<< (numBlocksPerDim.take(numDims), ", ") << ")"

    out << ">>>" << '(' <<< (arguments, ", ") << ')'
  }
}

case class CUDA_DeviceSynchronize() extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CUDA_DeviceSynchronize\n"

  override def expand : Output[Statement] = {
    CUDA_CheckError(FunctionCallExpression("cudaDeviceSynchronize", ListBuffer()))
  }
}

case class CUDA_Memcpy(var dest : Expression, var src : Expression, var sizeInBytes : Expression, var direction : String) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CUDA_Memcpy\n"

  override def expand : Output[Statement] = {
    CUDA_CheckError(
      FunctionCallExpression("cudaMemcpy",
        ListBuffer[Expression](dest, src, sizeInBytes, direction)))
  }
}

case class CUDA_Memset(var data : Expression, var value : Expression, var numElements : Expression, var datatype : Datatype) extends Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CUDA_Memset\n"

  override def expand : Output[Statement] = {
    CUDA_CheckError(FunctionCallExpression("cudaMemset", ListBuffer(data, value, numElements * SizeOfExpression(datatype))))
  }
}
