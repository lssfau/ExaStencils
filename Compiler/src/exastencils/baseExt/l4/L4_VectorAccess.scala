package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.baseExt.ir.IR_VectorExpression
import exastencils.datastructures.Node
import exastencils.prettyprinting.PpStream

/// L4_VectorExpression

// FIXME: to be replaced/ updated
object L4_VectorExpression {
  def apply(datatype : Option[L4_Datatype], expressions : List[L4_Expression], rowVector : Option[Boolean]) =
    new L4_VectorExpression(datatype, expressions.to[ListBuffer], rowVector)

  // helper function
  def isRowVector(n : Node) = {
    n match {
      case v : L4_VectorExpression => v.rowVector.getOrElse(true)
      case _                       => false
    }
  }
  def isColumnVector(n : Node) = {
    n match {
      case v : L4_VectorExpression => v.rowVector.getOrElse(true)
      case _                       => false
    }
  }
}

case class L4_VectorExpression(
    var datatype : Option[L4_Datatype],
    var expressions : ListBuffer[L4_Expression],
    var rowVector : Option[Boolean]) extends L4_Expression {
  // rowVector == true: Row; false: Column; None: unspecified

  def prettyprint(out : PpStream) = {
    out << '{' <<< (expressions, ", ") << '}'
    if (!rowVector.getOrElse(true)) out << 'T'
  }

  def progress = IR_VectorExpression(L4_ProgressOption(datatype)(_.progress), expressions.map(_.progress), rowVector)

  def length = expressions.length

  def apply(i : Integer) = expressions(i)
  def isConstant = expressions.count(_.isInstanceOf[L4_Number]) == expressions.length
  def convertConstants(dt : L4_Datatype): Unit = {
    expressions = expressions.map(exp => (exp, dt) match {
      case (c : L4_IntegerConstant, L4_RealDatatype | L4_FloatDatatype | L4_DoubleDatatype) => L4_RealConstant(c.v)
      case (c : L4_RealConstant, L4_IntegerDatatype) => L4_IntegerConstant(c.v.toInt)
      case (_, _) => println("Pair: " + exp + "," + dt); exp
    })
  }
}
