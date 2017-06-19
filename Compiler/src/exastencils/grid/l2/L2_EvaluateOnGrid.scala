package exastencils.grid.l2

import scala.collection.mutable._

import exastencils.base.l2._
import exastencils.grid.l3._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_EvaluateOnGrid

object L2_EvaluateOnGrid {
  def apply(name : String, level : Int, args : ListBuffer[L2_Expression]) = {
    if (0 == args.length) {
      Logger.warn(s"Trying to use build-in function $name without arguments")
      L2_RealConstant(0.0)
    } else {
      if (args.length > 2) Logger.warn(s"Trying to use build-in function $name with more than one arguments; additional arguments are discarded")
      args match {
        case ListBuffer(access : L2_Expression)                                    => new L2_EvaluateOnGrid(name, level, access, "default")
        case ListBuffer(access : L2_Expression, interpolation : L2_StringConstant) => new L2_EvaluateOnGrid(name, level, access, interpolation.value)
        case _                                                                     =>
          Logger.warn(s"Arguments (${ args.map(_.prettyprint).mkString(", ") }) are currently not supported for function $name")
          args(0)
      }
    }
  }
}

case class L2_EvaluateOnGrid(
    var name : String,
    var level : Int,
    var expression : L2_Expression,
    var interpolation : String,
    var offset : Option[L2_ConstIndex] = None) extends L2_Expression {

  override def prettyprint(out : PpStream) = out << name << '@' << level /* FIXME: offset */ << " ( " << expression << ", " << interpolation << " ) "
  override def progress = L3_EvaluateOnGrid(name, level, expression.progress, interpolation, L2_ProgressOption(offset)(_.progress))
}
