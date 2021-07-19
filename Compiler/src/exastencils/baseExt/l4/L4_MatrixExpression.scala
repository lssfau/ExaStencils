package exastencils.baseExt.l4

import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_Datatype
import exastencils.base.l4.L4_DoubleDatatype
import exastencils.base.l4.L4_Expression
import exastencils.base.l4.L4_FloatDatatype
import exastencils.base.l4.L4_IntegerConstant
import exastencils.base.l4.L4_IntegerDatatype
import exastencils.base.l4.L4_Number
import exastencils.base.l4.L4_ProgressOption
import exastencils.base.l4.L4_RealConstant
import exastencils.base.l4.L4_RealDatatype
import exastencils.baseExt.ir.IR_MatrixExpression
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_MatrixExpression

case class L4_MatrixExpression(
    var datatype : Option[L4_Datatype],
    var expressions : List[List[L4_Expression]],
    var shape : Option[L4_MatShape]
) extends L4_Expression {

  if (expressions.exists(_.length != expressions(0).length))
    Logger.error("Rows of matrix must be of equal length")

  def prettyprint(out : PpStream) = {
    out << "{ "
    expressions.foreach(out << "{ " <<< (_, ", ") << " }, ")
    out.removeLast(", ".length)
    out << " }"
    if (shape.isDefined) out << shape.get.toString()
  }

  override def progress = ProgressLocation(
    IR_MatrixExpression(
      L4_ProgressOption(datatype)(_.progress),
      this.rows,
      this.columns,
      expressions.flatten.map(_.progress).toArray,
      if (shape.isDefined) Some(shape.get.progress) else None
    )
  )

  def rows = expressions.length
  def columns = expressions(0).length
  def isConstant = expressions.flatten.count(_.isInstanceOf[L4_Number]) == expressions.length
  def convertConstants(dt : L4_Datatype) : Unit = {
    expressions = expressions.map(_.map(exp => (exp, dt) match {
      case (c : L4_IntegerConstant, L4_RealDatatype | L4_FloatDatatype | L4_DoubleDatatype) => L4_RealConstant(c.v)
      case (c : L4_RealConstant, L4_IntegerDatatype)                                        => L4_IntegerConstant(c.v.toInt)
      case (_, _)                                                                           => exp
    }))
  }
}
