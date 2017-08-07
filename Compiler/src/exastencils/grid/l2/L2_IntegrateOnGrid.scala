package exastencils.grid.l2

import scala.collection.mutable._

import exastencils.base.l2._
import exastencils.grid.l3._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_IntegrateOnGrid

object L2_IntegrateOnGrid {
  def apply(name : String, level : Int, args : ListBuffer[L2_Expression], offset : Option[L2_ConstIndex]) = {
    if (0 == args.length) {
      Logger.warn(s"Trying to use build-in function $name without arguments")
      L2_RealConstant(0.0)
    } else {
      if (args.length > 1) Logger.warn(s"Trying to use build-in function $name with more than one arguments; additional arguments are discarded")
      new L2_IntegrateOnGrid(name, level, args(0), offset)
    }
  }
}

case class L2_IntegrateOnGrid(
    var name : String,
    var level : Int,
    var expression : L2_Expression,
    var offset : Option[L2_ConstIndex]) extends L2_Expression {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << offset
    out << " ( " << expression << " ) "
  }

  override def progress = L3_IntegrateOnGrid(name, level, expression.progress, L2_ProgressOption(offset)(_.progress))
}
