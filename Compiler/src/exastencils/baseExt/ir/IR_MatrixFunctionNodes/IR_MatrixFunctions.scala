package exastencils.baseExt.ir.IR_MatrixFunctionNodes

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_HighDimAccess
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_Scope
import exastencils.base.ir.IR_UnitDatatype
import exastencils.base.ir.IR_VariableAccess
import exastencils.baseExt.ir.IR_BasicMatrixOperations
import exastencils.baseExt.ir.IR_GenerateBasicMatrixOperations
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.baseExt.ir.IR_MatrixExpression
import exastencils.baseExt.ir.IR_MatrixNodeUtilities
import exastencils.baseExt.ir.IR_PreItMOps
import exastencils.datastructures.Transformation.Output
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream


// transpose node for compiletime execution
object IR_Transpose {
  def apply(args : ListBuffer[IR_Expression]) = new IR_Transpose(args(0))
}
case class IR_Transpose(var arg : IR_Expression)
  extends IR_ExtractableMNode with IR_ResolvableMNode {
  def name = "transpose"
  override def datatype = arg.datatype
  override def prettyprint(out : PpStream) = out << "transpose" << '(' << arg << ')'
  override def resolve() : Output[IR_Expression] = {
    IR_BasicMatrixOperations.transpose(arg.asInstanceOf[IR_VariableAccess])
  }
  override def isResolvable() : Boolean = isExtractable() && !this.hasAnnotation(IR_PreItMOps.potentialInline)
  override def isExtractable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arg)
}

 // dot product node for compiletime execution
object IR_DotProduct {
  def apply(args : IR_Expression*) = new IR_DotProduct(args.to[ListBuffer])
}
case class IR_DotProduct(
    var arguments : ListBuffer[IR_Expression]
) extends IR_ExtractableMNode with IR_ResolvableMNode {
  def name = "dotProduct"
  override def datatype = arguments(0).datatype.resolveBaseDatatype
  override def prettyprint(out : PpStream) = out << "DotProduct" << '(' <<< (arguments, ", ") << ')'
  override def resolve() : Output[IR_Expression] = {
    IR_BasicMatrixOperations.dotProduct(arguments(0), arguments(1))
  }
//  override def isResolvable() : Boolean = !this.hasAnnotation(IR_ResolveMOps.potentialInline) && arguments.forall(arg => IR_MatrixNodeUtilities.isEvaluatable(arg))
  override def isResolvable() : Boolean = isExtractable() && !this.hasAnnotation(IR_PreItMOps.potentialInline)
  override def isExtractable() : Boolean = arguments.forall(arg => IR_MatrixNodeUtilities.isEvaluatable(arg))
}

  // cross product node for compiletime execution
object IR_CrossProduct {
  def apply(args : IR_Expression*) = new IR_CrossProduct(args.to[ListBuffer])
}
case class IR_CrossProduct(
    var arguments : ListBuffer[IR_Expression]
) extends IR_ExtractableMNode with IR_ResolvableMNode {
  def name = "crossProduct"
  override def datatype = IR_MatrixDatatype(arguments(0).datatype.resolveBaseDatatype, 3, 3)
  override def prettyprint(out : PpStream) = out << "CrossProduct" << '(' <<< (arguments, ", ") << ')'
  override def resolve() : Output[IR_Expression] = {
    IR_BasicMatrixOperations.crossProduct(arguments(0), arguments(1))
  }
  override def isResolvable() : Boolean = isExtractable() && !this.hasAnnotation(IR_PreItMOps.potentialInline)
  override def isExtractable() : Boolean = arguments.forall(arg => IR_MatrixNodeUtilities.isEvaluatable(arg))
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
  override def prettyprint(out : PpStream) = out << "Trace" << '(' << arg << ')'
  override def resolve() : Output[IR_Expression] = {
    IR_BasicMatrixOperations.trace(arg)
  }
  override def isResolvable() : Boolean = isExtractable() && !this.hasAnnotation(IR_PreItMOps.potentialInline)
  override def isExtractable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arg)
}

  // set element node for compiletime execution
object IR_SetElement {
  def apply(args : IR_Expression*) = new IR_SetElement(args.to[ListBuffer])
}
case class IR_SetElement(
    var arguments : ListBuffer[IR_Expression]
) extends IR_ExtractableMNode with IR_ResolvableMNode {
  def name = "setElement"
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) = out << "SetElement" << '(' <<< (arguments, ", ") << ')'
  override def resolve() : Output[IR_Scope] = {
    IR_Scope(IR_Assignment(IR_HighDimAccess(arguments(0), IR_ExpressionIndex(arguments(1), arguments(2))), arguments(3)))
  }
  override def isResolvable() : Boolean = isExtractable() && !this.hasAnnotation(IR_PreItMOps.potentialInline)
  override def isExtractable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arguments(0))
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
  override def isResolvable() : Boolean = isExtractable() && !this.hasAnnotation(IR_PreItMOps.potentialInline)
  override def isExtractable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arguments(0))
}

  // set slice node for compiletime execution
object IR_SetSlice {
  def apply(args : IR_Expression*) = new IR_SetSlice(args.to[ListBuffer])
}
case class IR_SetSlice(
    var arguments : ListBuffer[IR_Expression]
) extends IR_ExtractableMNode with IR_ResolvableMNode {
  def name = "setSlice"
  override def datatype = IR_UnitDatatype
  override def prettyprint(out : PpStream) = out << "setSlice" << '(' <<< (arguments, ", ") << ')'
  override def resolve() : Output[IR_Scope] = {
    var matrix = arguments(0)
    var offsetRows = arguments(1)
    var offsetCols = arguments(2)
    var nRows = arguments(3)
    var nCols = arguments(4)
    var newValue = arguments(5)
    if (IR_MatrixNodeUtilities.isScalar(newValue))
      IR_Scope(IR_GenerateBasicMatrixOperations.loopSetSubmatrixSc(matrix.asInstanceOf[IR_VariableAccess], offsetRows, offsetCols, nRows, nCols, newValue))
    else {
      var insize = IR_BasicMatrixOperations.getSize(newValue)
      newValue match {
        case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
          IR_Scope(IR_GenerateBasicMatrixOperations.loopSetSubmatrixMat(va, matrix.asInstanceOf[IR_VariableAccess], IR_IntegerConstant(insize._1), IR_IntegerConstant(insize._2), offsetRows, offsetCols))
        case x @ IR_MatrixExpression(_, _, _)                      =>
          var decl = IR_MatrixNodeUtilities.expressionToDeclaration(x,"slice_tmp_")
          IR_Scope(decl, IR_GenerateBasicMatrixOperations.loopSetSubmatrixMat(IR_VariableAccess(decl), matrix.asInstanceOf[IR_VariableAccess], IR_IntegerConstant(insize._1), IR_IntegerConstant(insize._2), offsetRows, offsetCols))
        case _                                                     => Logger.error(s"form of newValue matrix not supported: ${ newValue }, expected variable access to matrix variable")
      }
    }
  }
  override def isResolvable() : Boolean = isExtractable() && !this.hasAnnotation(IR_PreItMOps.potentialInline)
  override def isExtractable() : Boolean = arguments.forall(arg => IR_MatrixNodeUtilities.isEvaluatable(arg))
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