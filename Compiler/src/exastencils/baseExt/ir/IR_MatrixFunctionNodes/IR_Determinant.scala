package exastencils.baseExt.ir.IR_MatrixFunctionNodes

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_Scope
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_BasicMatrixOperations
import exastencils.baseExt.ir.IR_GenerateBasicMatrixOperations
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.baseExt.ir.IR_MatrixExpression
import exastencils.baseExt.ir.IR_MatrixNodeUtilities
import exastencils.datastructures.Transformation.Output
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream



// pre calculation det node: parse arguments(determine matrix size and depending on size execution in run/compiletime)
// and extract, will be transformed to IR_DeterminantCT or IR_DeterminantRT
object IR_Determinant {
  def apply(args : ListBuffer[IR_Expression]) = {
    var inMatrix = args(0) match {
      case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) => IR_MatrixNodeUtilities.accessToExpression(va)
      case x @ IR_MatrixExpression(_, _, _)                      => x
      case _                                                     => Logger.error(s"unexpected argument ${ args }, expected matrix expression or variable")
    }
    if (inMatrix.columns > 5)
      new IR_Determinant(inMatrix, true)
    else
      new IR_Determinant(inMatrix, false)
  }
}
case class IR_Determinant(
    var arg : IR_Expression,
    resolveAtRuntime : Boolean = false
) extends IR_RuntimeMNode {
  override def datatype = arg.datatype.resolveBaseDatatype
  override def prettyprint(out : PpStream) = Logger.error("internal node no resolved!")
  override def isExtractable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arg)
  override def name : String = "IR_Determinant"
}


// determinant node for compiletime execution
object IR_DeterminantCT {
  def apply(det : IR_Determinant) = new IR_DeterminantCT(det.arg)
}
case class IR_DeterminantCT(arg : IR_Expression) extends IR_Expression with IR_ResolvableMNode {
  override def datatype = arg.datatype.resolveBaseDatatype
  override def prettyprint(out : PpStream) = Logger.error("internal node no resolved!")
  override def resolve() : Output[IR_Expression] = {
    IR_BasicMatrixOperations.smallMatrixDeterminant(arg.asInstanceOf[IR_MatrixExpression])
  }
  override def isResolvable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arg)
}

// determinant node for runtime execution
object IR_DeterminantRT {
  def apply(dest : IR_VariableAccess, arg : IR_Expression) = new IR_DeterminantRT(dest, arg)
}
case class IR_DeterminantRT(dest : IR_VariableAccess, var arg : IR_Expression) extends IR_Expression with IR_ResolvableMNode {
  override def datatype = arg.datatype.resolveBaseDatatype
  override def prettyprint(out : PpStream) = Logger.error("internal node no resolved!")
  override def resolve() : Output[IR_Scope] = {
    arg match {
      case va @ IR_VariableAccess(_, IR_MatrixDatatype(_, _, _)) =>
        IR_GenerateBasicMatrixOperations.determinant(va, dest)
      case x @ IR_MatrixExpression(_, _, _)                      =>
        var stmts = IR_Scope(Nil)
        var tmp_access = IR_VariableAccess("detTmp_" + IR_MatrixExpression.matTmpCounter, x.datatype)
        stmts.body += IR_VariableDeclaration(tmp_access, x)
        stmts.body += IR_GenerateBasicMatrixOperations.determinant(tmp_access, dest)
        stmts
      case _                                                     => Logger.error(s"argument type not supported: ${ arg }, expected matrix expression or variable")
    }
  }
  override def isResolvable() : Boolean = IR_MatrixNodeUtilities.isEvaluatable(arg)
}