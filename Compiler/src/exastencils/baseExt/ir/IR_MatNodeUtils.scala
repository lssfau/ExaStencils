package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_Addition
import exastencils.base.ir.IR_ArrayAccess
import exastencils.base.ir.IR_Assignment
import exastencils.base.ir.IR_Division
import exastencils.base.ir.IR_DoubleConstant
import exastencils.base.ir.IR_DoubleDatatype
import exastencils.base.ir.IR_ElementwiseAddition
import exastencils.base.ir.IR_ElementwiseDivision
import exastencils.base.ir.IR_ElementwiseMultiplication
import exastencils.base.ir.IR_ElementwiseSubtraction
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_FloatConstant
import exastencils.base.ir.IR_FloatDatatype
import exastencils.base.ir.IR_FunctionCall
import exastencils.base.ir.IR_HighDimAccess
import exastencils.base.ir.IR_IntegerConstant
import exastencils.base.ir.IR_IntegerDatatype
import exastencils.base.ir.IR_Maximum
import exastencils.base.ir.IR_Minimum
import exastencils.base.ir.IR_Modulo
import exastencils.base.ir.IR_MultiDimArrayAccess
import exastencils.base.ir.IR_Multiplication
import exastencils.base.ir.IR_Negative
import exastencils.base.ir.IR_Node
import exastencils.base.ir.IR_NullExpression
import exastencils.base.ir.IR_Number
import exastencils.base.ir.IR_Power
import exastencils.base.ir.IR_RealConstant
import exastencils.base.ir.IR_RealDatatype
import exastencils.base.ir.IR_ReferenceDatatype
import exastencils.base.ir.IR_ScalarDatatype
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_StringConstant
import exastencils.base.ir.IR_StringDatatype
import exastencils.base.ir.IR_Subtraction
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.field.ir.IR_FieldAccess
import exastencils.field.ir.IR_MultiDimFieldAccess
import exastencils.logger.Logger

/** Strategy: methods to transform certain types of nodes related to matrices */
object IR_MatNodeUtils {
  /** Attribute: int give temporary variables unique names */
  var tmpCounter = 0

  /** Attribute: label a expression as evaluatable */
  val evaluatable : String = "evaluatable"

  /** Method: check if an argument is ready to be evaluated
    *
    * @param n : IR_Node, node to be checked for evaluatability
    * @return is evaluatable?
    * */
  //TODO other datatypes?
  def isEvaluatable(n : IR_Node) : Boolean = {
    if (n.hasAnnotation(evaluatable)) true
    else {
      n match {
        case x : IR_Expression if (isMatrix(x) | isMatFieldAccess(x) | isScalar(x) | isString(x) | isTensor(x)) =>
          n.annotate(evaluatable)
          true
        case _ : IR_Expression                                                            =>
          false
        case _                                                                            => Logger.error(s"unexpected type ${ n }")
      }
    }
  }

  def isMatrixWoFields(x : IR_Expression) : Boolean = {
    x match {
      case IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))                                                 => true
      case IR_MatrixExpression(_, _, _, _)                                                                  => true
      case IR_VariableAccess(_, IR_ReferenceDatatype(innerDt)) if (innerDt.isInstanceOf[IR_MatrixDatatype]) => true
      //FIXME this stmt enables SWE test: leave resolving multiplication of field accesses to IR_GeneralSimplify?
      case fa : IR_MultiDimFieldAccess if (fa.datatype.isInstanceOf[IR_MatrixDatatype])      => true
      case _                                                                                 => false
    }
  }

  /** Method: determine whether an expression is an access to a variable with type matrix or a matrix expression
    *
    * @param x : IR_Expression, expression to be checked for
    * @return is matrix?
    * */
  def isMatrix(x : IR_Expression) : Boolean = {
    x match {
      case IR_VariableAccess(_, IR_MatrixDatatype(_, _, _))                                                 => true
      case IR_MatrixExpression(_, _, _, _)                                                                  => true
      case IR_VariableAccess(_, IR_ReferenceDatatype(innerDt)) if (innerDt.isInstanceOf[IR_MatrixDatatype]) => true
      case fa : IR_MultiDimFieldAccess if (fa.datatype.isInstanceOf[IR_MatrixDatatype])      => true
      case _                                                                                 => false
    }
  }

  /** Method: determine whether an expression is an access to a variable with type tensor or a tensor expression
    *
    * @param x : IR_Expression, expression to be checked for
    * @return is tensor?
    * */
  def isTensor(x : IR_Expression) : Boolean = {
    x match {
      case IR_VariableAccess(_, _ : IR_TensorDatatype)                                                      => true
      case _ : IR_TensorExpression                                                                          => true
      case IR_VariableAccess(_, IR_ReferenceDatatype(innerDt)) if (innerDt.isInstanceOf[IR_TensorDatatype]) => true
      case _                                                                                                => false
    }
  }

  /** Method: determine whether an expression is an access to a scalar variable or constant/value
    *
    * @param x : IR_Expression, expression to be checked for
    * @return is scalar value?
    * */
  def isScalar(x : IR_Expression) : Boolean = {
    x match {
      case IR_VariableAccess(_, IR_RealDatatype | IR_IntegerDatatype | IR_DoubleDatatype | IR_FloatDatatype) => true
      case (IR_IntegerConstant(_) | IR_DoubleConstant(_) | IR_FloatConstant(_) | IR_RealConstant(_))         => true
      case IR_HighDimAccess(_, _)                                                                            => true
      case op @ (IR_Addition(_) | IR_Subtraction(_, _) | IR_Multiplication(_) | IR_Division(_, _) | IR_Modulo(_, _) | IR_Power(_, _)) //if (op.datatype.isInstanceOf[IR_ScalarDatatype]) => true
                                                                                                             =>
        val dt = op.datatype
        if (dt.isInstanceOf[IR_ScalarDatatype]) true
        else false
      case minmax @ (IR_Minimum(_) | IR_Maximum(_)) if (minmax.datatype.isInstanceOf[IR_ScalarDatatype])     => true
      case IR_VariableAccess(_, IR_ReferenceDatatype(innerDt)) if (innerDt.isInstanceOf[IR_ScalarDatatype])  => true
      case IR_ArrayAccess(_, _, _)                                                                           => true
      case IR_MultiDimArrayAccess(_, _)                                                                      => true
      case fa : IR_FieldAccess if (fa.datatype.isInstanceOf[IR_ScalarDatatype])                              => true
      case IR_Negative(x) if (x.datatype.isInstanceOf[IR_ScalarDatatype])                                    => true
      case _                                                                                                 => false
    }
  }

  /** Method: determine whether an expression is an access to a scalar variable or constant/value string
    *
    * @param x : IR_Expression, expression to be checked for
    * @return is string?
    * */
  def isString(x : IR_Expression) : Boolean = {
    x match {
      case IR_StringConstant(_)                    => true
      case IR_VariableAccess(_, IR_StringDatatype) => true
      case _                                       => false
    }
  }

  /** Attribute: label to mark an operation as matrix operation */
  val isMatOp = "isMatrixOperation"

  /** Attribute: label to mark an operation as non-matrix operation */
  val isNotMatOp = "isNotMatrixOperation"

  /** Method: check if an expression is a matrix operation or find out and label
    *
    * @param op : IR_Expression, expression to be checked
    * @return is mat op?
    * */
  def checkIfMatOp(op : IR_Expression) : Boolean = {
    if (op.hasAnnotation(isNotMatOp)) false
    else if (op.hasAnnotation(isMatOp)) true
    else {
      val b = op match {
      /*  case m : IR_Multiplication                                   => m.datatype.isInstanceOf[IR_MatrixDatatype]
        case a : IR_Addition                                         => a.datatype.isInstanceOf[IR_MatrixDatatype]
        case s : IR_Subtraction                                      => s.datatype.isInstanceOf[IR_MatrixDatatype]
        case s : IR_ElementwiseSubtraction                           => s.datatype.isInstanceOf[IR_MatrixDatatype]
        case m : IR_ElementwiseMultiplication                        => m.datatype.isInstanceOf[IR_MatrixDatatype]
        case e : IR_ElementwiseAddition                              => e.datatype.isInstanceOf[IR_MatrixDatatype]
        case e : IR_ElementwiseDivision                              => e.datatype.isInstanceOf[IR_MatrixDatatype]
        case _ @ IR_FunctionCall(ref, _) if (ref.name == "toMatrix") => true
        case _ @ IR_FunctionCall(ref, _) if (ref.name == "inverse")  => true
        case _ @ IR_FunctionCall(_, args)                            => args.exists(a => a.datatype.isInstanceOf[IR_MatrixDatatype] )
       */ case m : IR_Multiplication                                   => m.factors.exists(f => isMatrixWoFields(f))
        case a : IR_Addition                                         => a.summands.exists(f => isMatrix(f))
        case s : IR_Subtraction                                      => isMatrix(s.left) | isMatrix(s.right)
        case s : IR_ElementwiseSubtraction                           => isMatrix(s.left) | isMatrix(s.right)
        case e : IR_ElementwiseMultiplication                        => isMatrix(e.left) | isMatrix(e.right)
        case e : IR_ElementwiseAddition                              => isMatrix(e.left) | isMatrix(e.right)
        case e : IR_ElementwiseDivision                              => isMatrix(e.left) | isMatrix(e.right)
        case _ @ IR_FunctionCall(ref, _) if (ref.name == "toMatrix") => true
        case _ @ IR_FunctionCall(ref, _) if (ref.name == "inverse")  => true
        case _ @ IR_FunctionCall(_, args)                            => args.exists(a => isMatrix(a) | isMatFieldAccess(a))
      }
      if (b) {
        op.annotate(isMatOp)
        true
      }
      else {
        op.annotate(isNotMatOp)
        false
      }
    }
  }

  def isMatFieldAccess(expr : IR_Expression) : Boolean = {
    expr match {
      case fa : IR_FieldAccess if fa.field.layout.datatype.isInstanceOf[IR_MatrixDatatype] => true
      case _                                                                               => false
    }
  }


  /** Method: split a declaration with init to declaration and assignment with init
    *
    * @param decl : IR_VariableDeclaration, declaration to be split
    * @return list containing variable declaration without init and assignment of that variable with init expresion
    * */
  def splitDeclaration(decl : IR_VariableDeclaration) : ListBuffer[IR_Statement] = {
    val newStmts = ListBuffer[IR_Statement]()
    newStmts += IR_VariableDeclaration(decl.datatype, decl.name, None)
    newStmts += IR_Assignment(IR_VariableAccess(decl), decl.initialValue.getOrElse(IR_NullExpression))
    newStmts
  }

  /** Method: copy a matrix from an IR_Access to an IR_MatrixExpression
    * by building an expression of highDimAccesses
    *
    * @param src : IR_VariableAccess, access to convert
    * @return expression of hdas
    * */
  def accessToMatExpr(src : IR_Access) : IR_MatrixExpression = {
    var size = IR_CompiletimeMatOps.getSize(src)
    if (size._1 > 1 || size._2 > 1) {
      var out = IR_MatrixExpression(src.datatype.resolveBaseDatatype, size._1, size._2)
      for (i <- 0 until size._1) {
        for (j <- 0 until size._2) {
          out.set(i, j, IR_HighDimAccess(src, IR_ExpressionIndex(IR_IntegerConstant(i), IR_IntegerConstant(j))))
        }
      }
      out
    } else {
      IR_MatrixExpression(Some(src.datatype.resolveBaseDatatype), 1, 1, Array[IR_Expression](src))
    }
  }

  def exprToMatExpr(src : IR_Expression) : IR_MatrixExpression = {
    src match {
      case me : IR_MatrixExpression      => me
      case va : IR_VariableAccess        => accessToMatExpr(va)
      case mdfa : IR_MultiDimFieldAccess => accessToMatExpr(mdfa)
      case _                             => Logger.error(s"unexpected input expression: ${ src }")
    }
  }
  /** Method: transform a matrix expression to a temporary variable
    *
    * @param src  : IR_MatrixExpression, used as initialization
    * @param name : String, name of the new temporary variable
    * @return declaration of the tmp
    * */
  def expressionToDeclaration(src : IR_MatrixExpression, name : String) : IR_VariableDeclaration = {
    var decl = IR_VariableDeclaration(IR_MatrixDatatype(src.datatype.resolveBaseDatatype, src.rows, src.columns), name + tmpCounter, src)
    tmpCounter += 1
    decl
  }

  def toMatExpr(src : IR_Expression) : IR_MatrixExpression = {
    src match {
      case n : IR_Number           => IR_MatrixExpression(IR_MatrixDatatype(n.datatype, 1, 1), ListBuffer[IR_Expression](n))
      case x : IR_MatrixExpression => x
      case _                       => Logger.error(s"unexpected type: ${ src }")
    }
  }

}
