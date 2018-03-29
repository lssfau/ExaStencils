package exastencils.baseExt.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.baseExt.l4.L4_ColorLoops
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_ColorLoops

case class L3_ColorLoops(var colorExps : ListBuffer[L3_Modulo], var stmts : ListBuffer[L3_Statement]) extends L3_Statement {

  for (cExp <- colorExps)
    cExp.right match {
      case L3_IntegerConstant(i) if (i > 0) => // everything is fine
      case _                                =>
        Logger.error("the divisor of all color expressions for a color with statement must be a positive integer constant")
    }

  override def prettyprint(out : PpStream) = out << "color with {\n" <<< (colorExps, ",\n") << ",\n" <<< (stmts, "\n") << "\n}"
  override def progress = ProgressLocation(L4_ColorLoops(colorExps.map(_.progress), stmts.map(_.progress)))
}
