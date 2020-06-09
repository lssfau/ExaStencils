package exastencils.baseExt.ir.IR_MatrixFunctionNodes

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Datatype
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_Scope
import exastencils.base.ir.IR_UnknownDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.baseExt.ir.IR_BasicMatrixOperations
import exastencils.baseExt.ir.IR_GenerateBasicMatrixOperations
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.baseExt.ir.IR_MatrixNodeUtilities
import exastencils.datastructures.Transformation.Output
import exastencils.logger.Logger
import exastencils.optimization.ir.EvaluationException
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.prettyprinting.PpStream

// pre calculation slice node: parse arguments(determine argument availability for compiletime execution) and extract, will be transformed to IR_GetSliceCT or IR_GetSliceRT
object IR_GetSlice {
  def apply(args : ListBuffer[IR_Expression]) = {
    var evaluatable = true
    var args_asInts = ListBuffer[Int]()
    try {
      for(i <- 1 until 5)
        args_asInts += IR_SimplifyExpression.evalIntegral(args(i)).toInt
    } catch {
      case e : EvaluationException => evaluatable = false
      case t : Throwable           => Logger.error(s"unexpected exception: $t")
    }
    if (evaluatable)
      new IR_GetSlice(args, false, IR_MatrixDatatype(args(0).datatype.resolveBaseDatatype, args_asInts(2), args_asInts(3)))
    else {
      new IR_GetSlice(args, true)
    }
  }
}
case class IR_GetSlice(
    var arguments : ListBuffer[IR_Expression],
    resolveAtRuntime : Boolean,
    datatype : IR_Datatype = IR_UnknownDatatype
) extends IR_ExtractableMNode {
  // cant define datatype if length of slice is runtime dependent
  override def prettyprint(out : PpStream) = Logger.error("internal node no resolved!")
  override def isExtractable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arguments(0)) && !datatype.equals(IR_UnknownDatatype)
}


// slice node for compiletime execution
object IR_GetSliceCT {
  def apply(inMatrix : IR_Expression, args : ListBuffer[Int]) = {
    new IR_GetSliceCT(inMatrix, args)
  }
  def apply(gs : IR_GetSlice) = {
    var params = ListBuffer[Int]()
    for (i <- 1 until 5) params += IR_SimplifyExpression.evalIntegral(gs.arguments(i)).toInt
    new IR_GetSliceCT(gs.arguments(0), params)
  }
}
case class IR_GetSliceCT(
    inMatrix : IR_Expression,
    params : ListBuffer[Int]
) extends IR_Expression with IR_ResolvableMNode {
  override def datatype = IR_MatrixDatatype(inMatrix.datatype.resolveBaseDatatype, params(2), params(3))
  override def prettyprint(out : PpStream) = out << "getSliceCT(" << inMatrix << "," << params << ")"
  override def resolve() : Output[IR_Expression] = {
    IR_BasicMatrixOperations.copySubMatrix(inMatrix, params(0), params(1), params(2), params(3))
  }
  override def isResolvable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(inMatrix)
}

// slice node for runtime execution
object IR_GetSliceRT {
  def apply(dest : IR_VariableAccess, args : IR_Expression*) = new IR_GetSliceRT(dest, args.to[ListBuffer])
}
case class IR_GetSliceRT(dest : IR_VariableAccess, var args : ListBuffer[IR_Expression]) extends IR_Expression with IR_ResolvableMNode {
  // cant define datatype if length of slice is runtime dependent
  override def datatype = IR_UnknownDatatype
  override def prettyprint(out : PpStream) = Logger.error("internal node no resolved!")
  override def resolve() : Output[IR_Scope] = {
    IR_GenerateBasicMatrixOperations.loopCopySubmatrix(args(0), dest, args(1), args(2), args(3), args(4))
  }
  override def isResolvable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(args(0))
}