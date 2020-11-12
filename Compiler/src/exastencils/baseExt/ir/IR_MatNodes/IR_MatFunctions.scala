package exastencils.baseExt.ir.IR_MatNodes

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_HighDimAccess
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_Number
import exastencils.base.ir.IR_Scope
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.baseExt.ir.IR_CompiletimeMatOps
import exastencils.baseExt.ir.IR_MatNodeUtils
import exastencils.baseExt.ir.IR_MatOperations.IR_GenerateBasicMatrixOperations
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.baseExt.ir.IR_MatrixExpression
import exastencils.baseExt.ir.IR_TensorDatatype1
import exastencils.baseExt.ir.IR_TensorDatatype2
import exastencils.datastructures.Transformation.Output
import exastencils.field.ir.IR_MultiDimFieldAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

// transpose node for compiletime execution
object IR_Transpose {
  def apply(args : ListBuffer[IR_Expression]) = new IR_Transpose(IR_MatNodeUtils.exprToMatExpr(args(0)))
}

case class IR_Transpose(var arg : IR_Expression)
  extends IR_ExtractableMNode with IR_ResolvableMNode {
  def name = "transpose"
  override def datatype = IR_MatrixDatatype(arg.datatype.resolveBaseDatatype, arg.datatype.asInstanceOf[IR_MatrixDatatype].sizeM, arg.datatype.asInstanceOf[IR_MatrixDatatype].sizeN)
  override def prettyprint(out : PpStream) = out << "transpose" << '(' << arg << ')'
  override def resolve() : Output[IR_Expression] = {
    IR_CompiletimeMatOps.transpose(arg.asInstanceOf[IR_MatrixExpression])
  }
  override def isResolvable() : Boolean = IR_MatNodeUtils.isEvaluatable(arg)
  override def isExtractable() : Boolean = true
}

// dot product node for compiletime execution
object IR_DotProduct {
  def apply(args : ListBuffer[IR_Expression]) = {
    new IR_DotProduct(args(0), args(1))
  }
}

case class IR_DotProduct(
    var left : IR_Expression,
    var right : IR_Expression
) extends IR_ExtractableMNode with IR_ResolvableMNode {
  def name = "dotProduct"
  override def datatype = left.datatype.resolveBaseDatatype
  override def prettyprint(out : PpStream) = out << "dotProduct" << '(' << left << ", " << right << ')'
  override def resolve() : Output[IR_Expression] = {
    IR_CompiletimeMatOps.dotProduct(IR_MatNodeUtils.exprToMatExpr(left), IR_MatNodeUtils.exprToMatExpr(right))
  }
  //  override def isResolvable() : Boolean = !this.hasAnnotation(IR_ResolveMOps.potentialInline) && arguments.forall(arg => IR_MatrixNodeUtilities.isEvaluatable(arg))
  override def isResolvable() : Boolean = IR_MatNodeUtils.isEvaluatable(left) && IR_MatNodeUtils.isEvaluatable(right)
  override def isExtractable() : Boolean = true
}

// cross product node for compiletime execution
object IR_CrossProduct {
  def apply(args : ListBuffer[IR_Expression]) = new IR_CrossProduct(args(0), args(1))
}

case class IR_CrossProduct(
    var left : IR_Expression, right : IR_Expression
) extends IR_ExtractableMNode with IR_ResolvableMNode {
  def name = "crossProduct"
  override def datatype = IR_MatrixDatatype(left.datatype.resolveBaseDatatype, 3, 1)
  override def prettyprint(out : PpStream) = Logger.error("internal node not resolved")
  override def resolve() : Output[IR_Expression] = {
    IR_CompiletimeMatOps.crossProduct(IR_MatNodeUtils.exprToMatExpr(left), IR_MatNodeUtils.exprToMatExpr(right))
  }
  override def isResolvable() : Boolean = IR_MatNodeUtils.isEvaluatable(left) && IR_MatNodeUtils.isEvaluatable(right)
  override def isExtractable() : Boolean = true
}

// trace node for compiletime execution
object IR_Trace {
  def apply(args : ListBuffer[IR_Expression]) = new IR_Trace(args(0))
}

case class IR_Trace(
    var arg : IR_Expression
) extends IR_ExtractableMNode with IR_ResolvableMNode {
  def name = "trace"
  override def datatype = arg.datatype.resolveBaseDatatype
  override def prettyprint(out : PpStream) = out << "trace" << '(' << arg << ')'
  override def resolve() : Output[IR_Expression] = {
    IR_CompiletimeMatOps.trace(IR_MatNodeUtils.exprToMatExpr(arg))
  }
  override def isResolvable() : Boolean = IR_MatNodeUtils.isEvaluatable(arg)
  override def isExtractable() : Boolean = true
}

// set element node for compiletime execution
object IR_SetElement {
  def apply(args : IR_Expression*) = new IR_SetElement(args.to[ListBuffer])
}

case class IR_SetElement(
    var arguments : ListBuffer[IR_Expression]
) extends IR_Statement with IR_ResolvableMNode {
  def name = "setElement"
  override def prettyprint(out : PpStream) = out << "setElement" << '(' <<< (arguments, ", ") << ')'
  override def resolve() : Output[IR_Statement] = {
    IR_Assignment(IR_HighDimAccess(arguments(0), IR_ExpressionIndex(arguments(1), arguments(2))), arguments(3))
  }
  override def isResolvable() : Boolean = IR_MatNodeUtils.isEvaluatable(arguments(0))
}

// get element node for compiletime execution
object IR_GetElement {
  def apply(args : IR_Expression*) = new IR_GetElement(args.to[ListBuffer])
}

case class IR_GetElement(
    var arguments : ListBuffer[IR_Expression]
) extends IR_ExtractableMNode with IR_ResolvableMNode {
  def name = "getElement"
  override def datatype = arguments(0).datatype.resolveBaseDatatype
  override def prettyprint(out : PpStream) = out << "getElement" << '(' <<< (arguments, ", ") << ')'
  override def resolve() : Output[IR_Expression] = {
    IR_HighDimAccess(arguments(0), IR_ExpressionIndex(arguments(1), arguments(2)))
  }
  override def isResolvable() : Boolean = IR_MatNodeUtils.isEvaluatable(arguments(0))
  override def isExtractable() : Boolean = false
}

// set slice node for compiletime execution
object IR_SetSlice {
  def apply(args : IR_Expression*) = {
    (args(3), args(4)) match {
      case (r : IR_Number, c : IR_Number) if (r.asInstanceOf[IR_IntegerConstant].v == 1 && c.asInstanceOf[IR_IntegerConstant].v == 1) => IR_SetElement(args(0), args(1), args(2), args(5))
      case _                                                                                                                          => new IR_SetSlice(args.to[ListBuffer])
    }
  }
}

case class IR_SetSlice(
    var arguments : ListBuffer[IR_Expression]
) extends IR_Statement with IR_ResolvableMNode {
  def name = "setSlice"
  override def prettyprint(out : PpStream) = out << "setSlice" << '(' <<< (arguments, ", ") << ')'
  override def resolve() : Output[IR_Scope] = {
    var matrix = arguments(0)
    var offsetRows = arguments(1)
    var offsetCols = arguments(2)
    var nRows = arguments(3)
    var nCols = arguments(4)
    var newValue = arguments(5)

    if (IR_MatNodeUtils.isScalar(newValue))
      IR_Scope(IR_GenerateBasicMatrixOperations.loopSetSubmatrixSc(matrix, offsetRows, offsetCols, nRows, nCols, newValue))
    else {
      var insize = IR_CompiletimeMatOps.getSize(newValue)
      newValue match {
        case x : IR_MatrixExpression                                  =>
          var decl = IR_MatNodeUtils.expressionToDeclaration(x, "setSliceTmp_")
          IR_Scope(decl, IR_GenerateBasicMatrixOperations.loopSetSubmatrixMat(IR_VariableAccess(decl), matrix, IR_IntegerConstant(insize._1), IR_IntegerConstant(insize._2), offsetRows, offsetCols))
        case a @ (_ : IR_VariableAccess | _ : IR_MultiDimFieldAccess) =>
          IR_Scope(IR_GenerateBasicMatrixOperations.loopSetSubmatrixMat(a, matrix, IR_IntegerConstant(insize._1), IR_IntegerConstant(insize._2), offsetRows, offsetCols))
      }

    }
  }
  override def isResolvable() : Boolean = arguments.forall(arg => IR_MatNodeUtils.isEvaluatable(arg))
}

object IR_ToMatrix {
  def apply(arg : ListBuffer[IR_Expression]) = new IR_ToMatrix(arg(0))
}

case class IR_ToMatrix(
    var arg : IR_Expression
) extends IR_ExtractableMNode with IR_ResolvableMNode {
  def name = "toMatrix"
  override def datatype = arg.datatype match {
    case dt : IR_TensorDatatype2 => IR_MatrixDatatype(dt.datatype.resolveBaseDatatype, dt.dims, dt.dims)
    case dt : IR_TensorDatatype1 => IR_MatrixDatatype(dt.datatype.resolveBaseDatatype, dt.dims, 1)
    case _                       => Logger.error(s"unexpected datatype: ${ arg.datatype }")
  }
  override def prettyprint(out : PpStream) = out << "toMatrix" << '(' << arg << ')'
  override def resolve() : Output[IR_Expression] = {
    arg match {
      case t if (IR_MatNodeUtils.isTensor(t)) => IR_CompiletimeMatOps.convertTensorToMat(arg)
      case _                                  => Logger.error(s"cast to matrix not implemented yet for ${ arg }")
    }
  }
  override def isResolvable() : Boolean = IR_MatNodeUtils.isEvaluatable(arg)
  override def isExtractable() : Boolean = true
}

/*
object IR_MatMult {
  def apply(mult : IR_Multiplication) = {
    new IR_MatMult(mult)
  }
}
case class IR_MatMult(mult : IR_Multiplication) extends IR_MExpressionFunction {
  override def resolve() : IR_Expression = IR_BasicMatrixOperations.mult(mult)
  override def isResolvable() : Boolean =  mult.factors.forall(f => IR_MatrixNodeUtilities.isEvaluatable(f))
  override def datatype : IR_Datatype = mult.datatype
  override def prettyprint(out : PpStream) : Unit = Logger.error("internal node not resolved")
}
*/