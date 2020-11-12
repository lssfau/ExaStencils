package exastencils.baseExt.ir.IR_MatNodes

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_Scope
import exastencils.base.ir.IR_Statement
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_CompiletimeMatOps
import exastencils.baseExt.ir.IR_MatOperations.IR_GenerateBasicMatrixOperations
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.baseExt.ir.IR_MatrixExpression
import exastencils.baseExt.ir.IR_MatNodeUtils
import exastencils.datastructures.Transformation.Output
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/** Object: produce IR_Determinants */
object IR_Determinant {
  /** Method: apply
    *
    * @param args : ListBuffer[IR_Expression], matrix to calculate the determinant of
    *             @return Object of type IR_Determinant
    * */
  def apply(args : ListBuffer[IR_Expression]) = {
    var inMatrix = args(0) match {
      case va : IR_Access if(va.datatype.isInstanceOf[IR_MatrixDatatype]) => IR_MatNodeUtils.accessToMatExpr(va)
      case x @ IR_MatrixExpression(_,_, _, _)                      => x
      case _                                                     => Logger.error(s"unexpected argument ${ args }, expected matrix expression or variable")
    }
    if (inMatrix.columns > 5)
      new IR_Determinant(inMatrix, true)
    else
      new IR_Determinant(inMatrix, false)
  }
}
/** Case class: pre calculation det node: parse arguments(determine matrix size and depending on size execution in run/compiletime)
 and extract, will be transformed to IR_DeterminantCT or IR_DeterminantRT
  *
  * @param arg : IR_Expresion, matrix to calculate the determinant of
  * @param resolveAtRuntime : Boolean, should the calculation happen at runtime or compiletime
  */
case class IR_Determinant(
    var arg : IR_Expression,
    resolveAtRuntime : Boolean = false
) extends IR_RuntimeMNode {
  override def datatype = arg.datatype.resolveBaseDatatype
  override def prettyprint(out : PpStream) = Logger.error("internal node no resolved!")

  override def name : String = "determinant"
  override def isExtractable() : Boolean = true
}


/** Object: produce IR_DeterminantCTs */
object IR_DeterminantCT {
  /** Method: produce an IR_DeterminantCT from an IR_Determinant
    *
    * @param r : IR_RuntimeMNode, Determiant object to transform
    *            @return IR_DeterminantCT object with the same matrix
    * */
  def applyWithCheck(r : IR_RuntimeMNode) : IR_DeterminantCT = {
    r match {
      case det : IR_Determinant => new IR_DeterminantCT(det.arg)
      case _                    => Logger.error(s"unexpected type ${ r }, expected IR_Determinant")
    }
  }
}
/** Case class: Determinant to be resolved at Compiletime
  *
  * @param arg : IR_Expresion, matrix to calculate the determinant of
  */
case class IR_DeterminantCT(arg : IR_Expression) extends IR_Expression with IR_ResolvableMNode {
  override def datatype = arg.datatype.resolveBaseDatatype
  override def prettyprint(out : PpStream) = Logger.error("internal node no resolved!")
  /** Method: resolve IR_DeterminantCT node to the result of determinant calculation at compiletime
    *
    * @return determinant of matrix arg
    * */
  override def resolve() : Output[IR_Expression] = {
    IR_CompiletimeMatOps.smallMatrixDeterminant(arg.asInstanceOf[IR_MatrixExpression])
  }
  /** Method: check if this node is ready to be resolved: all arguments are available
    *
    * @return is argument available?
    * */
  override def isResolvable() : Boolean = IR_MatNodeUtils.isEvaluatable(arg)
}

/** Object: produce IR_DeterminantRTs */
object IR_DeterminantRT {
  /** Method: produce an IR_DeterminantRT from an IR_Determinant
    *
    * @param dest : IR_VariableAccess, variable the result of the calculation should be written to
    * @param src : IR_RuntimeMNod, matrix to calculate the determinant of
    *            @return IR_DeterminantRT object with the same matrix
    * */
  def applyWithCheck(dest : IR_Access, src : IR_RuntimeMNode) = {
    src match {
      case det : IR_Determinant => new IR_DeterminantRT(dest, det.arg)
      case _                    => Logger.error(s"unexpected type ${ src }, expected IR_Determinant")
    }
    }

}
/** Case class: Determinant to be resolved at Runtime
  *
  * @param dest : IR_VariableAccess, variable the result of the calculation should be written to
  * @param arg : IR_Expresion, matrix to calculate the determinant of
  */
case class IR_DeterminantRT(dest : IR_Access, var arg : IR_Expression) extends IR_Statement with IR_ResolvableMNode {
  override def prettyprint(out : PpStream) : Unit = Logger.error("internal node no resolved!")
  /** Method: resolve IR_DeterminantR node to the assignment of the result of determinant calculation to target variable
    *
    * @return statements to assign result to target variable
    * */
  override def resolve() : Output[IR_Scope] = {
    arg match {
      case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
        IR_GenerateBasicMatrixOperations.determinant(va, dest)
      case x @ IR_MatrixExpression(_, _, _,_)                      =>
        var stmts = IR_Scope(Nil)
        var tmp_access = IR_VariableAccess("detTmp_" + IR_MatrixExpression.matTmpCounter, x.datatype)
        stmts.body += IR_VariableDeclaration(tmp_access, x)
        stmts.body += IR_GenerateBasicMatrixOperations.determinant(tmp_access, dest)
        stmts
      case _                                                     => Logger.error(s"argument type not supported: ${ arg }, expected matrix expression or variable")
    }
  }
  /** Method: check if this node is ready to be resolved: all arguments are available
    *
    * @return is argument available?
    * */
  override def isResolvable() : Boolean = IR_MatNodeUtils.isEvaluatable(arg)
}