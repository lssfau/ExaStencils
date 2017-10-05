package exastencils.operator.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.field.ir._
import exastencils.prettyprinting.PpStream

/// IR_OperatorTimesField

case class IR_OperatorTimesField(var lhs : IR_OperatorAccess, var rhs : IR_FieldAccess) extends IR_Expression {
  override def datatype = rhs.datatype
  override def prettyprint(out : PpStream) = out << lhs << " * " << rhs
}

/// IR_ResolveOperatorTimesField

object IR_ResolveOperatorTimesField extends DefaultStrategy("Resolving IR operator field convolutions") {
  this += new Transformation("Resolve", {
    case mult @ IR_Multiplication(factors) =>
      val newFactors = ListBuffer[IR_Expression]()
      var skipNext = false
      for (i <- factors.indices) factors(i) match {
        case _ if skipNext => skipNext = false

        case op : IR_OperatorAccess =>
          if (i + 1 < factors.indices.length && factors(i + 1).isInstanceOf[IR_FieldAccess]) {
            newFactors += IR_OperatorTimesField(op, factors(i + 1).asInstanceOf[IR_FieldAccess])
            skipNext = true
          } else {
            newFactors += op
          }

        case other => newFactors += other
      }

      if (newFactors.length != factors.length) {
        IR_Multiplication(newFactors)
      } else {
        mult
      }
  })
}

/// IR_UnresolveOperatorTimesField

object IR_UnresolveOperatorTimesField extends DefaultStrategy("Revert stencil field convolutions to plain multiplications") {
  this += new Transformation("Replace", {
    case IR_OperatorTimesField(lhs, rhs) => IR_Multiplication(lhs, rhs)
  })
}
