package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_StringConstant
import exastencils.base.l4.L4_Expression
import exastencils.baseExt.ir.IR_MatShape
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

object L4_MatShape {
  def apply(args : ListBuffer[IR_Expression]) =
    new L4_MatShape(args)
}

case class L4_MatShape(
    args : ListBuffer[IR_Expression]
) extends L4_Expression {
  override def progress : IR_MatShape = ProgressLocation(IR_MatShape(args))
  override def prettyprint(out : PpStream) : Unit = {
    out << toString()
  }
  override def toString(): String = {
    var s = "{"
    if (args.length > 0) {
      val a = args.remove(0)
      a match {
        case IR_StringConstant(value) => s += value
        case _ => Logger.error("unexpected argument")
      }
    }
    for (a <- args) {
      a match {
        case IR_StringConstant(value) => s += "," + value
        case _ => Logger.error("unexpected argument")
      }
    }
    s += "}"
    s
  }
}
