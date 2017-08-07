package exastencils.grid.l3

import scala.collection.mutable._

import exastencils.base.l3._
import exastencils.grid.l4._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_IntegrateOnGrid

object L3_IntegrateOnGrid {
  def apply(name : String, level : Int, args : ListBuffer[L3_Expression], offset : Option[L3_ConstIndex]) = {
    if (0 == args.length) {
      Logger.warn(s"Trying to use build-in function $name without arguments")
      L3_RealConstant(0.0)
    } else {
      if (args.length > 1) Logger.warn(s"Trying to use build-in function $name with more than one arguments; additional arguments are discarded")
      new L3_IntegrateOnGrid(name, level, args(0), offset)
    }
  }
}

case class L3_IntegrateOnGrid(
    var name : String,
    var level : Int,
    var expression : L3_Expression,
    var offset : Option[L3_ConstIndex]) extends L3_Expression {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << offset
    out << " ( " << expression << " ) "
  }

  override def progress = L4_IntegrateOnGrid(name, level, expression.progress, L3_ProgressOption(offset)(_.progress))
}
