package exastencils.grid.l4

import scala.collection.mutable._

import exastencils.base.l4._
import exastencils.grid.ir._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_EvaluateOnGrid

object L4_EvaluateOnGrid {
  def apply(name : String, level : Int, args : ListBuffer[L4_Expression], offset : Option[L4_ConstIndex]) = {
    if (0 == args.length) {
      Logger.warn(s"Trying to use build-in function $name without arguments")
      L4_RealConstant(0.0)
    } else {
      if (args.length > 2) Logger.warn(s"Trying to use build-in function $name with more than one arguments; additional arguments are discarded")
      args match {
        case ListBuffer(access : L4_Expression) =>
          new L4_EvaluateOnGrid(name, level, access, "default", offset)

        case ListBuffer(access : L4_Expression, interpolation : L4_StringConstant) =>
          new L4_EvaluateOnGrid(name, level, access, interpolation.value, offset)

        case _                                                                     =>
          Logger.warn(s"Arguments (${ args.map(_.prettyprint).mkString(", ") }) are currently not supported for function $name")
          args(0)
      }
    }
  }
}

case class L4_EvaluateOnGrid(
    var name : String,
    var level : Int,
    var expression : L4_Expression,
    var interpolation : String,
    var offset : Option[L4_ConstIndex]) extends L4_Expression {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << offset
    out << " ( " << expression
    if ("default" != interpolation) out << ", " << interpolation
    out << " ) "
  }

  override def progress = IR_EvaluateOnGrid(name, level, expression.progress, interpolation, L4_ProgressOption(offset)(_.progress))
}
