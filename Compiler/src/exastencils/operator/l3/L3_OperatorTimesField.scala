package exastencils.operator.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.field.l3._
import exastencils.operator.l4.L4_OperatorTimesField
import exastencils.prettyprinting.PpStream

// TODO: is it really necessary to wrap convolutions in separate nodes?

/// L3_OperatorTimesField

case class L3_OperatorTimesField(var lhs : L3_OperatorAccess, var rhs : L3_FieldAccess) extends L3_Expression {
  def prettyprint(out : PpStream) = out << lhs << " * " << rhs
  def progress = L4_OperatorTimesField(lhs.progress, rhs.progress)
}

/// L3_ResolveOperatorTimesField

object L3_ResolveOperatorTimesField extends DefaultStrategy("Resolving L3 operator field convolutions") {
  this += new Transformation("Resolve", {
    case mult @ L3_Multiplication(factors) =>
      val newFactors = ListBuffer[L3_Expression]()
      var skipNext = false
      for (i <- factors.indices) factors(i) match {
        case _ if skipNext => skipNext = false

        case op : L3_OperatorAccess =>
          if (i + 1 < factors.indices.length && factors(i + 1).isInstanceOf[L3_FieldAccess]) {
            newFactors += L3_OperatorTimesField(op, factors(i + 1).asInstanceOf[L3_FieldAccess])
            skipNext = true
          } else {
            newFactors += op
          }

        case other => newFactors += other
      }

      if (newFactors.length != factors.length) {
        L3_Multiplication(newFactors)
      } else {
        mult
      }
  })
}
