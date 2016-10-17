package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.baseExt.ir.IR_MatrixExpression
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_MatrixExpression

// FIXME: to be replaced/ updated
object L4_MatrixExpression {
  def apply(datatype : Option[L4_Datatype], expressions : List[L4_VectorExpression])
  = new L4_MatrixExpression(datatype, expressions.to[ListBuffer])
}

case class L4_MatrixExpression(
    var datatype : Option[L4_Datatype],
    var expressions : ListBuffer[L4_VectorExpression]) extends L4_Expression {

  if (expressions.exists(_.length != expressions(0).length))
    Logger.error("Rows of matrix must be of equal length")

  def prettyprint(out : PpStream) = out << '{' <<< (expressions, ",\n") << "} '"
  def progress = IR_MatrixExpression(L4_ProgressOption(datatype)(_.progress), expressions.map(_.expressions.map(_.progress)))

  def rows = expressions.length
  def columns = expressions(0).length
  def isConstant = expressions.count(_.isConstant) == expressions.length
}
