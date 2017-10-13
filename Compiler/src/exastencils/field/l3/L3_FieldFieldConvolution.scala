package exastencils.field.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.datastructures._
import exastencils.field.l4.L4_FieldFieldConvolution
import exastencils.prettyprinting.PpStream

/// L3_FieldFieldConvolution

case class L3_FieldFieldConvolution(var lhs : L3_FieldAccess, var rhs : L3_FieldAccess) extends L3_Expression {
  override def prettyprint(out : PpStream) = out << lhs << " * " << rhs
  override def progress = L4_FieldFieldConvolution(lhs.progress, rhs.progress)
}

/// L3_ResolveFieldFieldConvolutions

object L3_ResolveFieldFieldConvolutions extends DefaultStrategy("Resolving L3 field field convolutions") {
  this += new Transformation("Resolve", {
    case fctCall : L3_FunctionCall if "dot" == fctCall.name =>
      fctCall.arguments match {
        case ListBuffer(lhs : L3_FieldAccess, rhs : L3_FieldAccess) => L3_FieldFieldConvolution(lhs, rhs)
        case _                                                      => fctCall
      }
  })
}
