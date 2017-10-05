package exastencils.operator.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.field.l4._
import exastencils.operator.ir.IR_OperatorTimesField
import exastencils.prettyprinting.PpStream

/// L4_OperatorTimesField

case class L4_OperatorTimesField(var lhs : L4_OperatorAccess, var rhs : L4_FieldAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = out << lhs << " * " << rhs
  def progress = IR_OperatorTimesField(lhs.progress, rhs.progress)
}

/// L4_ResolveOperatorTimesField

object L4_ResolveOperatorTimesField extends DefaultStrategy("Resolving L4 operator field convolutions") {
  this += new Transformation("Resolve", {
    case mult @ L4_Multiplication(factors) =>
      val newFactors = ListBuffer[L4_Expression]()
      var skipNext = false
      for (i <- factors.indices) factors(i) match {
        case _ if skipNext => skipNext = false

        case op : L4_OperatorAccess =>
          if (i + 1 < factors.indices.length && factors(i + 1).isInstanceOf[L4_FieldAccess]) {
            newFactors += L4_OperatorTimesField(op, factors(i + 1).asInstanceOf[L4_FieldAccess])
            skipNext = true
          } else {
            newFactors += op
          }

        case other => newFactors += other
      }

      if (newFactors.length != factors.length) {
        L4_Multiplication(newFactors)
      } else {
        mult
      }
  })
}

/// L4_UnresolveOperatorTimesField

object L4_UnresolveOperatorTimesField extends DefaultStrategy("Revert stencil field convolutions to plain multiplications") {
  this += new Transformation("Replace", {
    case L4_OperatorTimesField(lhs, rhs) => L4_Multiplication(lhs, rhs)
  })
}
