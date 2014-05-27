package exastencils.util

import exastencils.datastructures.ir.DivisionExpression
import exastencils.datastructures.ir.AdditionExpression
import exastencils.datastructures.ir.SubtractionExpression
import exastencils.datastructures.ir.MultiplicationExpression
import exastencils.datastructures.ir.IntegerConstant
import exastencils.datastructures.ir.Expression
import exastencils.datastructures.ir.FloatConstant

object EvaluateExpression {

  def integer(expr : Expression) : Long = expr match {
    case IntegerConstant(v)                                       => v
    case AdditionExpression(l : Expression, r : Expression)       => integer(l) + integer(r)
    case SubtractionExpression(l : Expression, r : Expression)    => integer(l) - integer(r)
    case MultiplicationExpression(l : Expression, r : Expression) => integer(l) * integer(r)
    case DivisionExpression(l : Expression, r : Expression)       => integer(l) / integer(r)
    case _ =>
      throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass())
  }

  def float(expr : Expression) : Double = expr match {
    case FloatConstant(v)                                         => v
    case AdditionExpression(l : Expression, r : Expression)       => float(l) + float(r)
    case SubtractionExpression(l : Expression, r : Expression)    => float(l) - float(r)
    case MultiplicationExpression(l : Expression, r : Expression) => float(l) * float(r)
    case DivisionExpression(l : Expression, r : Expression)       => float(l) / float(r)
    case _ =>
      throw new EvaluationException("unknown expression type for evaluation: " + expr.getClass())
  }
}

case class EvaluationException(msg : String) extends Exception(msg) {}
