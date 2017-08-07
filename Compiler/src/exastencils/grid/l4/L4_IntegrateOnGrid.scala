package exastencils.grid.l4

import scala.collection.mutable._

import exastencils.base.l4._
import exastencils.grid.ir._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_IntegrateOnGrid

object L4_IntegrateOnGrid {
  def apply(name : String, level : Int, args : ListBuffer[L4_Expression], offset : Option[L4_ConstIndex]) = {
    if (0 == args.length) {
      Logger.warn(s"Trying to use build-in function $name without arguments")
      L4_RealConstant(0.0)
    } else {
      if (args.length > 1) Logger.warn(s"Trying to use build-in function $name with more than one arguments; additional arguments are discarded")
      new L4_IntegrateOnGrid(name, level, args(0), offset)
    }
  }
}

case class L4_IntegrateOnGrid(
    var name : String,
    var level : Int,
    var expression : L4_Expression,
    var offset : Option[L4_ConstIndex]) extends L4_Expression {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << offset
    out << " ( " << expression << " ) "
  }

  override def progress = IR_IntegrateOnGrid(name, level, expression.progress, L4_ProgressOption(offset)(_.progress))
}
