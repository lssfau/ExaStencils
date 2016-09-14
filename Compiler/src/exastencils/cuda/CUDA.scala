package exastencils.cuda

import scala.collection.mutable._

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_MultiDimFieldAccess
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.ir._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.prettyprinting._
import exastencils.util._

// TODO: introduce abstraction layer for general device interfaces

trait CUDA_Statement extends IR_Statement

case class CUDA_Init() extends CUDA_Statement {
  override def prettyprint(out : PpStream) : Unit = out << "cuInit(0);"
}

case class CUDA_Finalize() extends CUDA_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    // has to be done after all other de-initialization statements
    out << "cuCtxDestroy(cudaContext);"
  }
}

case class CUDA_CheckError(var exp : IR_Expression) extends IR_Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CUDA_CheckError\n"

  override def expand() : Output[IR_Scope] = {
    // TODO: replace with define?
    IR_Scope(ListBuffer[IR_Statement](
      VariableDeclarationStatement(IR_SpecialDatatype("cudaError_t"), "cudaStatus", Some(exp)),
      IR_IfCondition(IR_NeqExpression("cudaStatus", "cudaSuccess"),
        PrintStatement(ListBuffer("\"CUDA error in file (\"", "__FILE__", "\"), line (\"", "__LINE__", "\"): \"", "cudaStatus",
          "\" -> \"", new FunctionCallExpression("cudaGetErrorString", "cudaStatus"), "std::endl")))))
  }
}

case class CUDA_AllocateStatement(var pointer : IR_Expression, var numElements : IR_Expression, var datatype : IR_Datatype) extends IR_Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CUDA_AllocateStatement\n"

  override def expand() : Output[IR_Statement] = {
    CUDA_CheckError(
      FunctionCallExpression("cudaMalloc",
        ListBuffer[IR_Expression](
          CastExpression(IR_PointerDatatype(IR_PointerDatatype(IR_UnitDatatype)), IR_AddressofExpression(pointer)),
          numElements * SizeOfExpression(datatype))))
  }
}

case class CUDA_FreeStatement(var pointer : IR_Expression) extends IR_Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CUDA_FreeStatement\n"

  override def expand() : Output[IR_Statement] = {
    IR_ExpressionStatement(new FunctionCallExpression("cudaFree", pointer))
  }
}

case class CUDA_UpdateHostData(var fieldAccess : IR_MultiDimFieldAccess) extends IR_Statement with Expandable {
  // TODO: allow targeting of specific index ranges

  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CUDA_UpdateHostData\n"

  override def expand() : Output[IR_IfCondition] = {
    val fieldSelection = fieldAccess.fieldSelection
    val field = fieldSelection.field
    IR_IfCondition(
      iv.DeviceDataUpdated(field, fieldSelection.slot),
      ListBuffer[IR_Statement](
        CUDA_Memcpy(
          iv.FieldData(field, fieldSelection.level, fieldSelection.slot),
          iv.FieldDeviceData(field, fieldSelection.level, fieldSelection.slot),
          (0 until field.fieldLayout.numDimsData).map(dim => field.fieldLayout.idxById("TOT", dim)).reduceLeft(_ * _)
            * SizeOfExpression(field.resolveBaseDatatype),
          "cudaMemcpyDeviceToHost"),
        IR_Assignment(iv.DeviceDataUpdated(field, fieldSelection.slot), IR_BooleanConstant(false))))
  }
}

case class CUDA_UpdateDeviceData(var fieldAccess : IR_MultiDimFieldAccess) extends IR_Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CUDA_UpdateDeviceData\n"

  override def expand() : Output[IR_IfCondition] = {
    val fieldSelection = fieldAccess.fieldSelection
    val field = fieldSelection.field
    IR_IfCondition(
      iv.HostDataUpdated(field, fieldSelection.slot),
      ListBuffer[IR_Statement](
        CUDA_Memcpy(
          iv.FieldDeviceData(field, fieldSelection.level, fieldSelection.slot),
          iv.FieldData(field, fieldSelection.level, fieldSelection.slot),
          (0 until field.fieldLayout.numDimsData).map(dim => field.fieldLayout.idxById("TOT", dim)).reduceLeft(_ * _)
            * SizeOfExpression(field.resolveBaseDatatype),
          "cudaMemcpyHostToDevice"),
        IR_Assignment(iv.HostDataUpdated(field, fieldSelection.slot), IR_BooleanConstant(false))))
  }
}

case class CUDA_FunctionCallExpression(
    var name : String,
    var arguments : ListBuffer[IR_Expression],
    var numThreadsPerDim : Array[IR_Expression],
    var numBlocksPerDim : Array[IR_Expression] = Knowledge.cuda_blockSizeAsVec.map(n => n : IR_Expression)) extends IR_Expression {

  def this(name : String, arguments : ListBuffer[IR_Expression], numThreadsPerDim : Array[Long]) = this(name, arguments, numThreadsPerDim.map(n => n : IR_Expression))
  def this(name : String, arguments : ListBuffer[IR_Expression], numThreadsPerDim : Array[Long], numBlocksPerDim : Array[Long]) = this(name, arguments, numThreadsPerDim.map(n => n : IR_Expression), numBlocksPerDim.map(n => n : IR_Expression))

  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = {
    val numDims = numThreadsPerDim.size
    if (numDims > 3) Logger.warn(s"${ numDims }D kernel found; this is currently unsupported by CUDA") // TODO: check relation to compute capability

    val numBlocks = (0 until numDims).map(dim => {
      (numThreadsPerDim(dim) + numBlocksPerDim(dim) - 1) / numBlocksPerDim(dim)
    }).toArray

    // TODO: simplify? check if integer ops are handled correctly

    out << name << "<<<"
    if (1 == numDims)
      out << numBlocks(0) << ", " << numBlocksPerDim(0) // only one dimensions -> wrapping not necessary
    else
      out << s"dim3(" <<< (numBlocks, ", ") << "), " << s"dim3(" <<< (numBlocksPerDim.take(numDims), ", ") << ")"

    out << ">>>" << '(' <<< (arguments, ", ") << ')'
  }
}

case class CUDA_FunctionCallExperimentalExpression(
    var name : String,
    var arguments : ListBuffer[IR_Expression],
    var numThreadsPerDim : Array[IR_Expression],
    var numBlocksPerDim : Array[IR_Expression] = Knowledge.cuda_blockSizeAsVec.map(n => n : IR_Expression)) extends IR_Expression {

  def this(name : String, arguments : ListBuffer[IR_Expression], numThreadsPerDim : Array[Long]) = this(name, arguments, numThreadsPerDim.map(n => n : IR_Expression))
  def this(name : String, arguments : ListBuffer[IR_Expression], numThreadsPerDim : Array[Long], numBlocksPerDim : Array[Long]) = this(name, arguments, numThreadsPerDim.map(n => n : IR_Expression), numBlocksPerDim.map(n => n : IR_Expression))

  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) : Unit = {
    val numDims = numThreadsPerDim.size
    if (numDims > 3) Logger.warn(s"${ numDims }D kernel found; this is currently unsupported by CUDA")

    out << name << "<<<"
    if (1 == numDims)
      out << numBlocksPerDim(0) << ", " << numThreadsPerDim(0) // only one dimensions -> wrapping not necessary
    else
      out << s"dim3(" <<< (numBlocksPerDim.take(numDims), ", ") << "), " << s"dim3(" <<< (numThreadsPerDim.take(numDims), ", ") << ")"

    out << ">>>" << '(' <<< (arguments, ", ") << ')'
  }
}

case class CUDA_DeviceSynchronize() extends IR_Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CUDA_DeviceSynchronize\n"

  override def expand() : Output[IR_Statement] = {
    CUDA_CheckError(FunctionCallExpression("cudaDeviceSynchronize", ListBuffer()))
  }
}

case class CUDA_Memcpy(var dest : IR_Expression, var src : IR_Expression, var sizeInBytes : IR_Expression, var direction : String) extends IR_Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CUDA_Memcpy\n"

  override def expand() : Output[IR_Statement] = {
    CUDA_CheckError(
      FunctionCallExpression("cudaMemcpy",
        ListBuffer[IR_Expression](dest, src, sizeInBytes, direction)))
  }
}

case class CUDA_Memset(var data : IR_Expression, var value : IR_Expression, var numElements : IR_Expression, var datatype : IR_Datatype) extends IR_Statement with Expandable {
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = CUDA_Memset\n"

  override def expand() : Output[IR_Statement] = {
    CUDA_CheckError(FunctionCallExpression("cudaMemset", ListBuffer(data, value, numElements * SizeOfExpression(datatype))))
  }
}

case class CUDA_SyncThreads() extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "__syncthreads();"
  }
}

case class CUDA_SharedArray(name : String, arrayType : IR_Datatype, var size : Array[Long]) extends IR_Statement {
  size = if (Knowledge.cuda_linearizeSharedMemoryAccess) Array(size.product) else size

  override def prettyprint(out : PpStream) : Unit = {
    out << "__shared__ " << arrayType << " " << name
    size.foreach(s => out << "[" << s << "]")
    out << ";"
  }
}

case class CUDA_UnsizedExternSharedArray(name : String, arrayType : IR_ScalarDatatype) extends IR_Statement {
  override def prettyprint(out : PpStream) : Unit = {
    out << "extern __shared__ " << arrayType << " " << name << "[];"
  }
}

case class CUDA_SharedArrayAccess(base : IR_Expression, indices : ListBuffer[IR_Expression], strides : IR_ExpressionIndex) extends IR_Access {
  def this(base : IR_Expression, indices : Array[IR_Expression], strides : IR_ExpressionIndex) = this(base, indices.to[ListBuffer], strides)

  override def datatype = base.datatype
  override def prettyprint(out : PpStream) : Unit = {
    out << base
    if (Knowledge.cuda_linearizeSharedMemoryAccess) {
      out << "[" << linearizeAccess() << "]"
    } else {
      indices.foreach(i => out << "[" << i << "]")
    }
  }

  def linearizeAccess() : IR_Expression = {
    Mapping.resolveMultiIdx(IR_ExpressionIndex(indices.toArray.reverse), strides : IR_ExpressionIndex)
  }
}

case class CUDA_MinimumExpression(left : IR_Expression, right : IR_Expression) extends IR_Expression {
  override def datatype = GetResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = {
    out << "min(" << left << "," << right << ")"
  }
}

case class CUDA_RestrictVariableAccess(var name : String, var dType : Option[IR_Datatype] = None) extends IR_Access {
  def this(n : String, dT : IR_Datatype) = this(n, Option(dT))
  override def datatype = dType.get
  // FIXME
  override def prettyprint(out : PpStream) : Unit = out << name

  def printDeclaration() : String = "const " + dType.get.resolveDeclType.prettyprint + " __restrict__ " + name + dType.get.resolveDeclPostscript
}
