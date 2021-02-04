package exastencils.baseExt.ir.IR_MatNodes

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Access
import exastencils.base.ir.IR_Expression
import exastencils.baseExt.ir.IR_CompiletimeMatOps
import exastencils.baseExt.ir.IR_MatNodeUtils
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.baseExt.ir.IR_MatrixExpression
import exastencils.datastructures.Transformation.OutputType
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

object IR_FrobeniusNorm {
  def apply(args : ListBuffer[IR_Expression]) = {
    var A = args(0) match {
      case va : IR_Access if(va.datatype.isInstanceOf[IR_MatrixDatatype]) => IR_MatNodeUtils.accessToMatExpr(va)
      case x @ IR_MatrixExpression(_,_, _, _)                      => x
      case _                                                     => Logger.error(s"unexpected argument ${ args }, expected matrix expression or variable")
    }
    new IR_FrobeniusNorm(A)
  }
}

case class IR_FrobeniusNorm (
    var A : IR_Expression,
) extends IR_ExtractableMNode with IR_ResolvableMNode {
  override def datatype = A.datatype.resolveBaseDatatype
  override def prettyprint(out : PpStream) = Logger.error("internal node no resolved!")

  override def name : String = "frobeniusNorm"
  override def isExtractable() : Boolean = true
  override def isResolvable() : Boolean = IR_MatNodeUtils.isEvaluatable(A)
  override def resolve() : OutputType = IR_CompiletimeMatOps.frobeniusNorm(IR_MatNodeUtils.exprToMatExpr(A))
}


