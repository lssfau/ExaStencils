package exastencils.baseExt.l4

import exastencils.base.l4._
import exastencils.baseExt.ir.IR_MatrixExpression
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_MatrixExpression

case class L4_MatrixExpression(
    var datatype : Option[L4_Datatype],
    var expressions : List[List[L4_Expression]]) extends L4_Expression {

  if (expressions.exists(_.length != expressions(0).length))
    Logger.error("Rows of matrix must be of equal length")

  def prettyprint(out : PpStream) = {
    out << '['
    expressions.foreach(out <<< (_, " "))
    out << ']'
  }
  def progress = IR_MatrixExpression(L4_ProgressOption(datatype)(_.progress), this.rows, this.columns, expressions.flatten.map(_.progress).toArray)

  def rows = expressions.length
  def columns = expressions(0).length
  def isConstant = expressions.flatten.count(_.isInstanceOf[L4_Number]) == expressions.length
  def convertConstants(dt : L4_Datatype) : Unit = {
    expressions = expressions.map(_.map(exp => (exp, dt) match {
      case (c : L4_IntegerConstant, L4_RealDatatype | L4_FloatDatatype | L4_DoubleDatatype) => L4_RealConstant(c.v)
      case (c : L4_RealConstant, L4_IntegerDatatype)                                        => L4_IntegerConstant(c.v.toInt)
      case (_, _)                                                                           => println("Pair: " + exp + "," + dt); exp
    }))
  }
}
