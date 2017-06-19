package exastencils.grid.l3

import scala.collection.mutable._

import exastencils.base.l3._
import exastencils.grid.l4._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_EvaluateOnGrid

object L3_EvaluateOnGrid {
  def apply(name : String, level : Int, args : ListBuffer[L3_Expression]) = {
    if (0 == args.length) {
      Logger.warn(s"Trying to use build-in function $name without arguments")
      L3_RealConstant(0.0)
    } else {
      if (args.length > 2) Logger.warn(s"Trying to use build-in function $name with more than one arguments; additional arguments are discarded")
      args match {
        case ListBuffer(access : L3_Expression)                                    => new L3_EvaluateOnGrid(name, level, access, "default")
        case ListBuffer(access : L3_Expression, interpolation : L3_StringConstant) => new L3_EvaluateOnGrid(name, level, access, interpolation.value)
        case _                                                                     =>
          Logger.warn(s"Arguments (${ args.map(_.prettyprint).mkString(", ") }) are currently not supported for function $name")
          args(0)
      }
    }
  }
}

case class L3_EvaluateOnGrid(
    var name : String,
    var level : Int,
    var expression : L3_Expression,
    var interpolation : String,
    var offset : Option[L3_ConstIndex] = None) extends L3_Expression {

  override def prettyprint(out : PpStream) = out << name << '@' << level /* FIXME: offset */ << " ( " << expression << ", " << interpolation << " ) "
  override def progress = L4_EvaluateOnGrid(name, level, expression.progress, interpolation, L3_ProgressOption(offset)(_.progress))
}
